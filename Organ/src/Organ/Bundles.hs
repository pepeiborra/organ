{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Organ.Bundles
  ( Input(..)
  , sources
  , traverseInput
  , Output(..)
  , sinks
  , Funnel(..)
  , Unfunnel(..)
  , Organ.Bundles.drain
  , Organ.Bundles.tee
  , dup
  )where

import           Control.Concurrent
import           Control.Lens                               as Lens
import           Control.Lens.Extras
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.Finite
import qualified Data.Foldable                              as F
import           Data.List.NonEmpty                         (NonEmpty (..),
                                                             nonEmpty)
import           Data.Semigroup
import qualified Data.Vector.Sized                          as V
import qualified Data.Vector.Unboxed.Mutable.Indexed.Length as UM
import           GHC.TypeLits
import           Organ

newtype Input m ar a = Input {_sources :: m (V.Vector ar (Src (m ()) a))}

newtype Output m ar a = Output {_sinks :: m (V.Vector ar (Snk (m()) a))}

makeLenses ''Input
makeLenses ''Output

instance Functor m => Functor (Input m ar) where
  fmap = over (sources.mapped.mapped.sets mapSrc)

instance (KnownNat ar, Monad m, Monoid (m())) => Applicative (Input m ar) where
  pure x = Input {_sources = return $ V.replicate (Organ.cons x empty)}
  f <*> x = Input $ do
      fs <- _sources f
      xs <- _sources x
      return $ fmap (mapSrc (uncurry ($)) . uncurry zipSrc) (V.zip fs xs)

instance (KnownNat ar, MonadIO m, Monoid (m())) => Monoid (Input m ar a) where
  mempty = Input {_sources = return $ V.replicate empty}
  mappend inp1 inp2 = Input $ do
      srcs1 <- inp1 ^. sources
      srcs2 <- liftIO $ newMVar Nothing
      let wrap :: Int -> Src (m()) a -> Src (m()) a
          wrap _ src full@Full{} = src full
          wrap i src (Cont c) = src $ Cont $ \case
              Cons a rest -> c $ Cons a (wrap i rest)
              Done Nothing -> do
                srcs2v <- liftIO $ takeMVar srcs2
                case srcs2v of
                  x@(Just srcs2v) -> do
                    liftIO $ putMVar srcs2 x
                    V.unsafeIndex srcs2v i (Cont c)
                  Nothing -> do
                    srcs2v <- inp2 ^. sources
                    liftIO $ putMVar srcs2 (Just srcs2v)
                    V.unsafeIndex srcs2v i (Cont c)
              error@Done{} -> c error
      return $ V.imap wrap srcs1

class (KnownNat ar) => Funnel stream (ar :: Nat) where
  funnel :: stream ar a -> stream 1 a

instance (KnownNat ar, Functor m, Monoid (m())) => Funnel (Input m) ar where
  -- | Returns a new source that consumes the argument sources in sequence
  funnel = over (sources.mapped) (V.singleton . F.fold)

instance (KnownNat ar, Functor m, Monoid (m())) => Funnel (Output m) ar where
  funnel = over (sinks.mapped) (V.singleton . F.fold)

class (KnownNat ar) => Unfunnel stream (ar :: Nat) where
  unfunnel :: KnownNat ar' => stream ar a -> stream ar' a

traverseInput :: MonadCatch m => (a -> m b) -> Input m ar a -> Input m ar b
traverseInput f = over (sources.mapped.mapped) (mapSrcM f)

instance (MonadIO m, PrimMonad m) => Unfunnel (Input m) 1 where
  unfunnel :: forall ar a m.
              (KnownNat ar, MonadIO m, PrimMonad m) =>
              Input m 1 a -> Input m ar a
  -- | Returns a new bundle where every source pulls from the argument source
  --   * The sources in the bundle ignore Full sinks and so the
  --     argument source will only release resources when fully depleted
  unfunnel = over sources $ \start -> do
    (V.toList->[src]) <- start
    state   <- liftIO $ newMVar src
    aborted :: UM.MVector ar _ Bool <- UM.replicate False
    -- When all the children have seen a Full, we want to notify the original source
    let run :: Finite ar -> Src (m()) a
        run i x@Full{} = do
          UM.write aborted i True
          ifAllThen aborted $ src x
        run i (Cont k) = do
          src <- liftIO $ takeMVar state
          src $ Cont $ \source ->
              case source of
                x@Done{} -> do
                  liftIO $ putMVar state src
                  k x
                Cons a src' -> do
                  liftIO $ putMVar state src'
                  k $ Cons a (run i)
    return $ V.generate_ run

instance (MonadIO m, PrimMonad m) => Unfunnel (Output m) 1 where
  unfunnel
    :: forall ar a m.
       (KnownNat ar, MonadIO m, PrimMonad m)
    => Output m 1 a -> Output m ar a
  -- | Returns a bundle of sinks that all push to the argument sink
  --   The argument sink is done when all the result sinks are done
  unfunnel out =
    Output $ do
      (V.toList -> [parent]) <- _sinks out
      arr <- UM.replicate False
      state <- liftIO $ newMVar parent
      let run :: Finite ar -> Snk (m ()) a
          run i x@Done{} = do
            UM.write arr i True
            isLast <- andVector arr
            when isLast $ do
              sink <- liftIO $ readMVar state
              sink x
          run i (Cons a src) = do
            sink <- liftIO $ takeMVar state
            sink $
              Cons a $ \snk ->
                case snk of
                  x@Full{} -> do
                    liftIO $ putMVar state sink
                    src x
                  Cont sink' -> do
                    liftIO $ putMVar state sink'
                    src $ Cont (run i)
      return $ V.generate_ run


tee :: (Monad m, Monoid(m())) => Output m ar a -> Input m ar a -> Input m ar a
tee out = over sources $ \start -> do
  srcs <- start
  snks <- out^.sinks
  return $ fmap (uncurry Organ.tee) (V.zip srcs snks)

-- | Drain a source to exhaustion (or fullness) in 'ar' threads
drain :: KnownNat ar => Input IO ar a -> Output IO ar a -> IO ()
drain inp out = do
  srcs <- inp^.sources
  snks <- out^.sinks
  mvs <- forM (V.toList srcs `zip` V.toList snks) $ \(s,t) -> do
    mv <- newEmptyMVar
    _  <- forkFinally (fwd s t) $ \res -> do
            putMVar mv ()
            case res of
              Left e  -> throwM e
              Right _ -> return ()
    return mv
  mapM_ takeMVar mvs

dup :: (MonadIO m) => Input m ar a -> m (Input m ar a, Input m ar a)
dup inp = do
  srcs <- inp^.sources
  dups <- traverse dupSrc srcs
  let (inp1, inp2) = V.unzip dups
  return (inp & sources .~ return inp1, inp & sources .~ return inp2)

data DupState m a = DupState
  { aborted :: Maybe (Either () ())
  , buffer  :: Maybe (Either (NonEmpty a) (NonEmpty a))
  , parent  :: Src m a
  }

dupSrc
  :: forall m a .
  (MonadIO m)
  => Src (m ()) a -> m (Src (m ()) a, Src (m ()) a)
dupSrc src = do
  state <- liftIO $ newMVar $ DupState Nothing Nothing src
  let run :: (forall b. Prism' (Either b b) b) -> Src (m()) a
      run me full@Full{} = do
        s@DupState{..} <- liftIO $ takeMVar state
        case aborted of
          Just x | is me x ->
            -- Nothing to do, me has already been aborted
            liftIO $ putMVar state s
          Just _ -> do
            -- The other one has been aborted, and now me, so abort the parent
            liftIO $ putMVar state s
            parent full
          Nothing -> do
            let buffer'
                  | Just x <- buffer
                  , is me x = Nothing
                  | otherwise = buffer
            liftIO $ putMVar state DupState{ aborted = Just (review me ())
                                           , buffer = buffer'
                                           , ..}
      run me (Cont k) = do
        s@DupState{..} <- liftIO $ takeMVar state
        case buffer of
          -- Catch up with other src
          Just (preview me -> Just (x :| rest)) -> do
            liftIO $ putMVar state $ s{buffer = fmap (review me) (nonEmpty rest)}
            k $ Cons x (run me)
          _ -> -- Consume and buffer
            parent $ Cont $
              \case
                x@Done{} -> do
                  liftIO $ putMVar state s
                  k x
                Cons a parent' -> do
                  let buffer'
                       | Just _ <- aborted = buffer
                       | Just b <- buffer = Just $ b & me %~ ([a] <>)
                       | otherwise = Just (review me [a])
                  liftIO $ putMVar state s{buffer = buffer', parent = parent'}
                  k $ Cons a (run me)
  return (run _Left, run _Right)


andVector
  :: forall n m.
     (KnownNat n, PrimMonad m)
  => UM.MVector n (PrimState m) Bool -> m Bool
andVector arr = loop maxBound where
  loop :: Finite n -> m Bool
  loop i = case i of
           0 -> UM.read arr i
           _ -> UM.read arr i >>= \x -> if x then loop (i-1) else return False

ifAllThen
  :: forall n m.
     (PrimMonad m, KnownNat n)
  => UM.MVector n (PrimState m) Bool -> m () -> m ()
ifAllThen vector cont = loop_ifAllThen maxBound
  where
  loop_ifAllThen :: Finite n -> m ()
  loop_ifAllThen i =
    case i of
      0 -> cont
      _ -> do
        v <- UM.read vector i
        when v $ loop_ifAllThen (i - 1)
