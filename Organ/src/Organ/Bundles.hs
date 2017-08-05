{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
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
  ( Sources(..)
  , _Sources
  , Organ.Bundles.join
  , traverseSources
  , Sinks(..)
  , _Sinks
  , Funnel(..)
  , Unfunnel(..)
  , Organ.Bundles.drain
  , Organ.Bundles.tee
  , dup
  )where

import           Control.Concurrent
import           Control.Lens                               as Lens
import           Control.Monad                              as Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.Finite
import qualified Data.Foldable                              as F
import qualified Data.Vector.Sized                          as V
import qualified Data.Vector.Unboxed.Mutable.Indexed.Length as UM
import           GHC.TypeLits
import           Organ

type BundleType m ar stream = m (V.Vector ar stream)

newtype Sources m ar a  = Sources  {sources :: BundleType m ar (Src (m()) a)}
newtype Sinks m ar a = Sinks {sinks :: BundleType m ar (Snk (m()) a)}

join :: Monad m => m (Sources m ar a) -> Sources m ar a
join = Sources . Monad.join . fmap sources

makePrisms ''Sources
makePrisms ''Sinks
makeWrapped ''Sources
makeWrapped ''Sinks

instance Functor m => Functor (Sources m ar) where
  fmap = over (_Sources.mapped.mapped.sets mapSrc)

instance (KnownNat ar, Monad m, Monoid (m())) => Applicative (Sources m ar) where
  pure x = Sources $ return $ V.replicate (Organ.cons x empty)
  f <*> x = Sources $ do
      fs <- sources f
      xs <- sources x
      return $ fmap (mapSrc (uncurry ($)) . uncurry zipSrc) (V.zip fs xs)

instance (KnownNat ar, MonadIO m, Monoid (m())) => Monoid (Sources m ar a) where
  mempty = Sources $ return $ V.replicate empty
  mappend inp1 inp2 = Sources $ do
      srcs1 <- sources inp1
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
                    srcs2v <- sources inp2
                    liftIO $ putMVar srcs2 (Just srcs2v)
                    V.unsafeIndex srcs2v i (Cont c)
              error@Done{} -> c error
      return $ V.imap wrap srcs1

class (KnownNat ar) => Funnel stream (ar :: Nat) where
  funnel :: stream ar a -> stream 1 a

instance (KnownNat ar, Functor m, Monoid (m())) => Funnel (Sources m) ar where
  -- | Returns a new source that consumes the argument sources in sequence
  funnel = over (_Sources.mapped) (V.singleton . F.fold)

instance (KnownNat ar, Functor m, Monoid (m())) => Funnel (Sinks m) ar where
  funnel = over (_Sinks.mapped) (V.singleton . F.fold)

class (KnownNat ar, KnownNat ar') => Unfunnel stream (ar :: Nat) ar' where
  unfunnel :: KnownNat ar' => stream ar a -> stream ar' a

traverseSources :: MonadCatch m => (a -> m b) -> Sources m ar a -> Sources m ar b
traverseSources f = over (_Sources.mapped.mapped) (traverseSrc f)

instance KnownNat ar => Unfunnel (Sources m) ar ar where unfunnel = id

instance (MonadIO m, PrimMonad m, KnownNat ar) => Unfunnel (Sources m) 1 ar where
  -- | Returns a new bundle where every source pulls from the argument source
  --   * The sources in the bundle ignore Full sinks and so the
  --     argument source will only release resources when fully depleted
  unfunnel inp = Sources $ do
    (V.toList->[src]) <- sources inp
    state   <- liftIO $ newMVar src
    aborted :: UM.MVector ar _ Bool <- UM.replicate False
    -- When all the children have seen a Full, we want to notify the original source
    let run :: Finite ar -> Src (m()) _
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

instance KnownNat ar => Unfunnel (Sinks m) ar ar where unfunnel = id

instance (MonadIO m, PrimMonad m, KnownNat ar) => Unfunnel (Sinks m) 1 ar where
  -- | Returns a bundle of sinks that all push to the argument sink
  --   The argument sink is done when all the result sinks are done
  unfunnel out =
    Sinks $ do
      (V.toList -> [parent]) <- sinks out
      arr <- UM.replicate False
      state <- liftIO $ newMVar parent
      let run :: Finite ar -> Snk (m ()) _
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


tee :: (Monad m, Monoid(m())) => Sinks m ar a -> Sources m ar a -> Sources m ar a
tee out inp = Sources $ do
  srcs <- sources inp
  snks <- sinks out
  return $ fmap (uncurry Organ.tee) (V.zip srcs snks)

-- | Drain a source to exhaustion (or fullness) in 'ar' threads
drain :: KnownNat ar => Sources IO ar a -> Sinks IO ar a -> IO ()
drain inp out = do
  srcs <- sources inp
  snks <- sinks out
  mvs <- forM (V.toList srcs `zip` V.toList snks) $ \(s,t) -> do
    mv <- newEmptyMVar
    _  <- forkFinally (fwd s t) $ \res -> do
            putMVar mv ()
            case res of
              Left e  -> throwM e
              Right _ -> return ()
    return mv
  mapM_ takeMVar mvs

dup :: (MonadIO m) => Sources m ar a -> m (Sources m ar a, Sources m ar a)
dup inp = do
  srcs <- sources inp
  dups <- traverse dupSrc srcs
  let (inp1, inp2) = V.unzip dups
  return (Sources (return inp1), Sources (return inp2))

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
