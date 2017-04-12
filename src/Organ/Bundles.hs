{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Organ.Bundles where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Vec
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits
import           Organ

newtype Sources ar m a = Sources (Vec ar (Src m a))
newtype Sinks   ar m a = Sinks (Vec ar (Snk m a))

-- | Returns a new source that consumes the argument sources in sequence
funnel :: Monoid m => Sources m a -> Src m a
funnel (Sources ss)= mconcat ss

-- | Returns a new bundle where every source pulls from the argument source
--   * The sources in the bundle ignore Full sinks and so the
--     argument source will only release resources when fully depleted
unfunnel_i :: Int -> Src (IO()) a -> IO(Sources (IO()) a)
unfunnel_i n src = do
  state   <- newMVar src
  aborted <- UM.new n ; UM.set aborted False
  -- When all the children have seen a Full, we want to notify the original source
  let run i Full = do
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
  return $ Sources $ map run [0 .. n-1]

-- | Returns a bundle of sinks that all push to the argument sink
--   The argument sink is done when all the result sinks are done
unfunnel_o :: forall a. Int -> Snk (IO ()) a -> IO(Sinks (IO ()) a)
unfunnel_o n parent = do
  arr <- UM.replicate n False
  state <- newMVar parent
  let run :: Int -> Snk (IO()) a
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
  return $ Sinks $ map run [0..n-1]

drainP :: Sources (IO()) a -> Sinks (IO()) a -> IO ()
drainP (Sources srcs) (Sinks snks) = do
  mvs <- forM (zip srcs snks) $ \(s,t) -> do
    mv <- newEmptyMVar
    _  <- forkFinally (fwd s t) (\_ -> putMVar mv ())
    return mv

  mapM_ takeMVar mvs

andVector arr = loop (UM.length arr - 1) where
  loop 0 = UM.read arr 0
  loop n = UM.read arr n >>= \x -> if x then loop (n-1) else return False

ifAllThen vector cont = loop_ifAllThen (UM.length vector) where
  loop_ifAllThen 0 = cont
  loop_ifAllThen (pred -> n) = do
    v <- UM.read vector n
    when v $ loop_ifAllThen n

-- dubious version of unfunnel_o that does not refresh the sink cont
-- unfunnel_o n parent = do
--   arr <- UM.new n
--   UM.set arr False
--   let run i Done = do
--         UM.write arr i True
--         isLast <- andVector arr
--         when isLast $ parent Done
--       run i (Cons a k) = do
--         parent (Cons a (\_ -> return ()))
--         k $ Cont (run i)

--   return $ Sinks $ map run [0..n-1]
