{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module Organ.Streams where

import           Control.Arrow
import           Control.Arrow.Kleisli.Class
import           Control.Category            (Category)
import qualified Control.Category
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           GHC.TypeLits
import           Organ.Bundles

newtype Stream m ar a b = Stream (Input m ar a -> m (Input m ar b))

instance (KnownNat ar, Monad m) => Category (Stream m ar) where
  id = Stream return
  Stream s1 . Stream s2 = Stream (s2 >=> s1)

instance (KnownNat ar, MonadIO m, Monoid (m())) => Arrow (Stream m ar) where
  arr f = Stream (return . fmap f)
  first (Stream f) = Stream $ \ab -> do
    (fmap fst -> a, fmap snd -> b) <- dup ab
    a' <- f a
    return $ (,) <$> a' <*> b

instance (KnownNat ar, MonadIO m, MonadCatch m, Monoid (m ())) =>
         ArrowKleisli m (Stream m ar) where
  arrM f = Stream (return . traverseInput f)

instance (KnownNat ar, MonadIO m, Monoid(m())) => ArrowZero (Stream m ar) where
  zeroArrow = Stream $ \_ -> return mempty

instance (KnownNat ar, MonadIO m, Monoid(m())) => ArrowPlus (Stream m ar) where
  Stream s1 <+> Stream s2 = Stream $ \i -> do
    i1 <- s1 i
    let i2 = Input $ join $ view sources <$> s2 i
    return (i1 `mappend` i2)
