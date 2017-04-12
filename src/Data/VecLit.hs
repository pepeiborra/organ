{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.VecLit (Vec, ) where

import Data.Maybe
import Data.Proxy
import GHC.Exts
import GHC.TypeLits

newtype Vec (n :: Nat) a =
  Vec { down :: [a] }
  deriving (Eq, Show, Ord, Applicative, Functor, Foldable)

instance Traversable (Vec n) where
  traverse f (Vec []) = Vec <$> pure []
  traverse f (Vec (x:xx)) = (Vec.) . (:) <$> f x <*> traverse f xx

instance KnownNat n => IsList (Vec n a) where
  type Item (Vec n a) = a
  fromList = fromMaybe (error "Not enough elements") . up
  toList = down

up :: forall n a. KnownNat n => [a] -> Maybe(Vec n a)
up = fmap Vec . takeN (natVal (Proxy @ n))
  where
    takeN 0 _  = Just []
    takeN _ [] = Nothing
    takeN i (x:xx) = (x:) <$> takeN (i-1) xx
