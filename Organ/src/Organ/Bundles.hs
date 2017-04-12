{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Organ.Bundles
  ( Sources
  , Sinks
  , funnel_i
  , unfunnel_i
  , funnel_o
  , unfunnel_o
  , drainP
  )where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Finite
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed.Mutable.Indexed.Length as UM
import qualified Data.Vector.Sized as V
import           GHC.TypeLits
import           Organ

newtype Sources ar m a = Sources (V.Vector ar (Src m a))
newtype Sinks   ar m a = Sinks (V.Vector ar (Snk m a))

-- | Returns a new source that consumes the argument sources in sequence
funnel_i :: KnownNat ar => Monoid m => Sources ar m a -> Sources 1 m a
funnel_i (Sources ss)= Sources (V.singleton $ F.fold ss)

-- | Returns a new sink that
funnel_o :: (KnownNat ar, Monoid m) => Sinks ar m a -> Sinks 1 m a
funnel_o (Sinks ss) = Sinks (V.singleton $ F.fold ss)

-- | Returns a new bundle where every source pulls from the argument source
--   * The sources in the bundle ignore Full sinks and so the
--     argument source will only release resources when fully depleted
unfunnel_i
  :: forall ar a.
     KnownNat ar
  => Sources 1 (IO ()) a -> IO (Sources ar (IO ()) a)
unfunnel_i (Sources(V.toList -> [src])) = do
  state   <- newMVar src
  aborted :: UM.MVector ar _ Bool <- UM.replicate False
  -- When all the children have seen a Full, we want to notify the original source
  let run :: Finite ar -> Src (IO()) a
      run i Full = do
        UM.write aborted i True
        ifAllThen aborted $ src Full
      run i (Cont k) = do
        src <- takeMVar state
        src $ Cont $ \source ->
            case source of
              Done -> do
                putMVar state src
                k Done
              Cons a src' -> do
                putMVar state src'
                k $ Cons a (run i)
  return $ Sources $ V.generate_ run
unfunnel_i _ = unreachable

-- | Returns a bundle of sinks that all push to the argument sink
--   The argument sink is done when all the result sinks are done
unfunnel_o
  :: forall ar a.
     KnownNat ar
  => Sinks 1 (IO ()) a -> IO (Sinks ar (IO ()) a)
unfunnel_o  (Sinks(V.toList -> [parent])) = do
  arr <- UM.replicate False
  state <- newMVar parent
  let run :: Finite ar -> Snk (IO()) a
      run i Done = do
        UM.write arr i True
        isLast <- andVector arr
        when isLast $ do
          sink <- takeMVar state
          sink Done
      run i (Cons a src) = do
        sink <- takeMVar state
        sink $ Cons a $ \snk ->
          case snk of
            Full -> src Full
            Cont sink' -> do
              putMVar state sink'
              src $ Cont (run i)
  return $ Sinks $ V.generate_ run
unfunnel_o _ = unreachable

-- | Drain a source to exhaustion (or fullness)
drainP :: KnownNat ar => Sources ar (IO()) a -> Sinks ar (IO()) a -> IO ()
drainP (Sources srcs) (Sinks snks) = do
  mvs <- forM (V.toList srcs `zip` V.toList snks) $ \(s,t) -> do
    mv <- newEmptyMVar
    _  <- forkFinally (fwd s t) (\_ -> putMVar mv ())
    return mv

  mapM_ takeMVar mvs

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

unreachable = error "unreachable"
