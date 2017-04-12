{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-#OPTIONS_GHC -fplugin GHC.TypeLits.Normalise#-}
module Data.Vec where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

data Vec (n::Nat) a where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n+1) a

instance Show a => Show (Vec n a) where show = show . down
instance Eq a => Eq (Vec n a) where v == v' = down v == down v'

down :: Vec n a -> [a]
down Nil = []
down (Cons a v) = a : down v

up :: forall n a. KnownNat n => [a] -> Maybe(Vec n a)
up [] | Just Refl <- sameNat (Proxy @ 0) (Proxy @ n)= Just Nil
up (x:xx) | Just (vxx :: Vec (n-1) a) <- up xx = Just (Cons x vxx)
up _ = Nothing
