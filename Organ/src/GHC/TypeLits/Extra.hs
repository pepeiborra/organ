{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GHC.TypeLits.Extra
  ( module GHC.TypeLits
  , intVal
  ) where

import Data.Proxy
import GHC.TypeLits

intVal :: forall n . KnownNat n => Int
intVal = fromIntegral(natVal (Proxy @ n))
