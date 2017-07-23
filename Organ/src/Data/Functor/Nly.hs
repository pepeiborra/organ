{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Functor.Nly where

import           Control.Lens

-- -------------------------------
-- Functors on function arguments
-- -------------------------------
newtype Nly b f a = Nly {nly :: f a -> b}

makeWrapped ''Nly
makePrisms  ''Nly

instance Functor f => Contravariant (Nly m f) where
  contramap ba (Nly fa) = Nly (fa . fmap ba)

instance Contravariant f => Functor (Nly m f) where
  fmap f (Nly g) = Nly (g . contramap f)
