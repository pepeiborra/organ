{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
module Duality where

import           Control.Newtype
import           Data.Functor.Contravariant
import           GHC.Generics

type Eff = IO ()
type N a = a -> Eff
type NN a = N(N a)

shift :: a -> NN a
shift x k = k x

unshift :: N (NN a) -> N a
unshift k x = k (shift x)

data Source a = Done  | Cons a (N (Sink a))
data Sink a   = Full | Cont (N (Source a))

instance Functor Source where
  fmap _ Done       = Done
  fmap f (Cons a k) = Cons (f a) (k . contramap f)

instance Contravariant Sink where
  contramap _ Full     = Full
  contramap f (Cont k) = Cont (k . fmap f)

-- breaks linearity ? I don't understand
await :: Source a -> Eff -> (a -> Source a -> Eff) -> Eff
await Done eof _      = eof
await (Cons x cs) _ k = cs $ Cont $ \xs -> k x xs

yield :: a -> Sink a -> (Sink a -> Eff) -> Eff
yield x (Cont c) k = c (Cons x k)
yield _ Full k     = k Full

eject :: Sink a -> Eff
eject Full     = mempty
eject (Cont c) = c Done

drain :: Source a -> Sink a -> Eff
drain s (Cont s')     = s' s
drain Done Full       = mempty
drain (Cons _ k) Full = k Full

type Src a = N (Sink a)   -- Sink a -> Eff
type Snk a = N (Source a) -- Src  a -> Eff

mapSrc :: (a -> b) -> Src a -> Src b
mapSrc f = nly . (fmap f) . Nly

newtype Nly f a = Nly {nly :: forall b. f a -> b}
-- instance Newtype (Nly f a) where type O(Nly f a) = forall b. f a -> b ; pack = Nly ; unpack = nly

instance Functor f => Contravariant (Nly f) where
  contramap ba (Nly fa) = Nly $ \fb -> fa $ fmap ba fb

instance Contravariant f => Functor (Nly f) where
  fmap f (Nly g) = Nly (\fa -> g $ contramap f fa)
