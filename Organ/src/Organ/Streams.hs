{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module Organ.Streams where

import           Control.Arrow
import           Control.Arrow.Kleisli.Class
import           Control.Category            (Category)
import qualified Control.Category
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           GHC.TypeLits
import           Organ.Bundles as Bundles

newtype SourcesA m ar a b = SourcesA (Sources m ar a -> Sources m ar b)

instance (KnownNat ar, Monad m) => Category (SourcesA m ar) where
  id = SourcesA id
  SourcesA s1 . SourcesA s2 = SourcesA (s1 . s2)

instance (KnownNat ar, MonadIO m, Monoid (m())) => Arrow (SourcesA m ar) where
  arr f = SourcesA (fmap f)
  first (SourcesA f) = SourcesA $ \ab -> Bundles.join $ do
    (fmap fst -> a, fmap snd -> b) <- dup ab
    return $ (,) <$> f a <*> b

instance (KnownNat ar, MonadIO m, MonadCatch m, Monoid (m ())) =>
         ArrowKleisli m (SourcesA m ar) where
  arrM f = SourcesA (traverseSources f)

instance (KnownNat ar, MonadIO m, Monoid(m())) => ArrowZero (SourcesA m ar) where
  zeroArrow = SourcesA $ \_ -> mempty

instance (KnownNat ar, MonadIO m, Monoid(m())) => ArrowPlus (SourcesA m ar) where
  SourcesA s1 <+> SourcesA s2 = SourcesA $ \i -> s1 i `mappend` s2 i
