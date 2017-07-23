{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Organ.Streams where

import           Control.Arrow
import           Control.Category       (Category)
import qualified Control.Category
import           Control.Monad
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

