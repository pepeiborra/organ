{-# LANGUAGE BangPatterns, TupleSections, RankNTypes, TypeOperators, GADTs, LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module Organ.Fusion.Source where

import Data.Bifunctor

-- There are four choices of representation:

-- Nu for sources. In this case the sources respond to data

import qualified Data.Foldable as F
import System.IO
import GHC.Types ( SPEC(..) )

type Eff = IO ()
type N a = a -> Eff
type NN a = N (N a) -- (a->Eff) -> Eff

-- This is the underlying functor; but with a delay monad slapped on
-- it, so we can implement filter and friends without an (explicit)
-- loop.
data Step a s = Nil
              | Delay s
              | Cons a s
data Choke s = Full | More s

data Source a = forall s. Source s (s -> N (Choke (N (Step a s))))

{-# INLINE mapS #-}
{-# INLINE mapF #-}
mapS f = bimap id f
mapF f = bimap f id

instance Bifunctor Step where
  {-# INLINE bimap #-}
  bimap _ _ Nil = Nil
  bimap _ f (Delay x) = Delay (f x)
  bimap g f (Cons a s) = Cons (g a) (f s)

type Sink a = Snk a
type Snk a = N (Source a)
type Src a = NN (Source a)

{-# INLINE shift #-}
shift :: a -> NN a
shift x k = k x

mapNN :: (a -> b) -> NN a -> NN b
mapNN f k1 k2 = k1 $ \ a -> k2 (f a)
 
unshift :: N (NN a) -> N a
unshift k x = k (shift x)

{-# INLINE forward #-}
forward :: Source a -> Sink a -> Eff
forward s s' = s' s

onSource   :: (Src  a -> t) -> Source   a -> t
onSink     :: (Snk  a -> t) -> Sink   a -> t

{-# INLINE onSource #-}
{-# INLINE onSink #-}

onSource  f   s = f   (\t -> forward s t)
onSink    f   t = f   (\s -> forward s t)

unshiftSnk :: N (Src a) -> Snk a
unshiftSrc :: N (Snk a) -> Src a
shiftSnk :: Snk a -> N (Src a)
shiftSrc :: Src a -> N (Snk a)

unshiftSnk = onSource
unshiftSrc = onSink
shiftSnk k kk = kk ( k)
shiftSrc k kk = k ( kk)

{-# INLINE shiftSrc #-}
{-# INLINE shiftSnk #-}

fwd :: Src a -> Snk a -> Eff
fwd src k = src k

flipSnk :: (Snk a -> Snk b) -> Src b -> Src a
flipSnk f s = shiftSrc s . onSink f

{-# INLINE flipSrc #-}
{-# INLINE flipSnk #-}

flipSrc :: (Src a -> Src b) -> Snk b -> Snk a
flipSrc f t = shiftSnk t . onSource f

{-# INLINE mapSrc #-}
{-# INLINE mapSnk #-}
mapSrc  :: (a -> b) -> Src  a -> Src  b
mapSrc f = flipSnk (mapSnk f)

mapSnk  :: (b -> a) -> Snk  a -> Snk b
-- mapSnk f t (Source s0 psi) = t (Source s0  (\s -> mapNN (mapF f) (psi s)))
mapSnk f t (Source s0 psi) = t $ Source s0 $ \s -> \case
  Full -> psi s Full
  More u -> psi s $ More $ u . mapF f

nnIntro :: Src a -> Src (NN a)
nnIntro = mapSrc shift

nnElim' :: Snk (NN a) -> Snk a
nnElim' = mapSnk shift

{-# INLINE toListSrc #-}  
{-# INLINE toListSnk #-}  
toListSrc :: Src a -> NN [a]
toListSrc s k = shiftSrc s (toListSnk k)

-- HELP I don't know how to write a right fold
toListSnk :: N [a] -> Snk a
toListSnk k (Source s0 step) = loopSnk SPEC k s0 where
 loopSnk !_ k s0 = step s0 $ More $ \case
  Nil -> k []
  Delay s -> loopSrc SPEC s $ \res -> k res
  Cons a s -> loopSrc SPEC s $ \res -> k (a : res)

 loopSrc !_ acc s0 = undefined

-- FIXME this is doing a left fold, which is a bad way of building a list
toListSnk_bad :: N [a] -> Snk a
toListSnk_bad k (Source s0 step) = loop SPEC k s0 where
 loop !_ acc s0 = step s0 $ More $ \case
  Nil -> k []
  Delay s -> loop SPEC k s
  Cons a s -> loop SPEC ( k . (a:)) s

{-# INLINE lengthSrc #-}
{-# INLINE lengthSnk #-}
lengthSrc :: Src a -> NN Int
lengthSrc s k = shiftSrc s (lengthSnk k)

lengthSnk :: N Int -> Snk a
lengthSnk k (Source s0 step) = loop SPEC 0 s0 where
  loop !_ !acc s0 = step s0 $ More $ \case
    Nil -> k acc
    Delay s -> loop SPEC acc s
    Cons _ s -> loop SPEC (acc+1) s

nnElim :: Src (NN a) -> Src a
nnElim = flipSnk nnIntro'

nnIntro' :: Snk a -> Snk (NN a)
nnIntro' k (Source s0 psi) = k $ Source s0 $ \s -> \case
  Full -> psi s Full
  More k' -> psi s $ More $ \case
    Nil -> k' Nil
    Delay xs -> k' (Delay xs)
    Cons x xs -> x $ \x' -> k' (Cons x' xs)


takeSnk  :: Int -> Snk  a -> Snk  a
takeSnk n t (Source s0 psi) = t $ Source (n,s0) $ 
  \(m,s) -> \case
    Full -> psi s Full
    More k -> case m of
      0 -> k Nil
      _ -> psi s $ More $ \case
        Nil -> k Nil
        Delay xs -> k $ Delay (m,xs)
        Cons x xs -> k $ Cons x (m-1,xs)

{-# INLINE dropSrc #-}
{-# INLINE dropSnk #-}
dropSnk :: Int -> Snk a -> Snk a
dropSnk n t (Source s0 step) = t $ Source (n,s0) $
  \(n,s) -> \case
    Full -> step s Full
    More k -> case n of
      0 -> step s (More (k . mapS (0,)))
      _ -> step s $ More $ \case
        Nil -> k Nil
        Delay xs -> k $ Delay (n,xs)
        Cons _ xs -> k $ Delay (n-1,xs)

dropSrc :: Int -> Src a -> Src a
dropSrc n = flipSnk (dropSnk n)

filterSnk :: (a->Bool) -> Snk a -> Snk a
filterSnk p t (Source s0 step) = t $ Source s0 $
  \s -> \case
    Full -> step s Full
    More k -> step s $ More $ \case
      Nil -> k Nil
      Delay s' -> k $ Delay s'
      Cons x xs
       | p x -> k $ Cons x xs
       | otherwise -> k$ Delay xs

{-# INLINE filterSrc #-}
{-# INLINE filterSnk #-}
filterSrc p = flipSnk(filterSnk p)

hFileSnk :: Handle -> Snk String
hFileSnk h (Source s0 psi) = psi s0 $ More $ \case 
  Nil -> hClose h
  Cons c as -> do
    hPutStrLn h c
    hFileSnk h (Source as psi)
      -- this is a final consumer (driver of
      -- computation): it is ok to have a loop
      -- here.

hFileSrc :: Handle -> Src String
hFileSrc h c = c $ Source () $ \()-> \case
  Full -> hClose h
  More k -> do
    e <- hIsEOF h
    if e then do hClose h
                 k Nil
         else do x <- hGetLine h
                 k (Cons x ())

empty :: Src a
-- empty k = k $ Source () (\_ -> \k -> k Nil)
empty k = k $ Source () $ \_ -> \case
  More k -> k Nil
  Full -> mempty

{-# INLINE each #-}
each :: Foldable t => t a -> Src a
-- each = foldr cons empty
each xs = shift $ Source (F.toList xs) step where
  step [] (More k) = k Nil
  step _ Full = mempty
  step (x:xs) (More k) = k $ Cons x xs

{-# INLINE enumFromTo #-}
enumFromTo :: Int -> Int -> Src Int
enumFromTo from to = shift $ Source from step where
  step n (More k) | n == to = k Nil
  step _ Full = mempty
  step n (More k) = k $ Cons n (n+1)

cons :: a -> Src a -> Src a
cons a = flipSnk (push a)

consH :: a -> Source a -> Source a
consH a (Source s step) = Source Nothing step'
  where
    step' _ Full = step s Full
    step' Nothing  (More k) = k (Cons a (Just s))
    step' (Just s) (More k) = step s (More (k . mapS Just))

push :: a -> Snk a -> Snk a
push a k = k . consH a

full :: Snk a
full (Source s step) = step s Full

plug :: Snk a
plug source = forward source full

-- "Loops"
type Schedule a = Source a -> Source (N a) -> Eff

sequentially :: Schedule a
sequentially (Source s1 psi1) (Source s2 psi2) =
  psi1 s1 $ More $ \i1 ->
  psi2 s2 $ More $ \i2 ->
  case (i1,i2) of
   (Cons x1 xs1, Cons x2 xs2) -> do
     x2 x1
     sequentially (Source xs1 psi1) (Source xs2 psi2)
