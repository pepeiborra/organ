{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Vector.Unboxed.Mutable.Indexed.Length
  ( MVector
  , Unbox
  , new
  , down
  , replicate
  , read
  , write
  ) where

import Control.Monad.Primitive
import Data.Finite
import Data.Vector.Unboxed.Mutable (Unbox)
import qualified Data.Vector.Unboxed.Mutable as VM
import GHC.TypeLits.Extra
import Prelude hiding (read, replicate)

newtype MVector (n :: Nat) s a =
  MVector { down :: VM.MVector s a }

new :: forall n m a . (KnownNat n, PrimMonad m, Unbox a) => m (MVector n (PrimState m) a)
new = MVector <$> VM.new (intVal @ n)

replicate
  :: forall n m a.
     (KnownNat n, PrimMonad m, Unbox a)
  => a -> m (MVector n (PrimState m) a)
replicate x = MVector <$> VM.replicate (intVal @ n) x

read
  :: forall n m a.
     (PrimMonad m, Unbox a)
  => MVector n (PrimState m) a -> Finite n -> m a
read (MVector m) i = VM.unsafeRead m (fromIntegral $ getFinite i)

write
  :: forall n m a.
     (PrimMonad m, Unbox a)
  =>  MVector n (PrimState m) a -> Finite n -> a -> m ()
write (MVector m) i x = VM.unsafeWrite m (fromIntegral $ getFinite i) x
