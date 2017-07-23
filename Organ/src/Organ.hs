{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fdefer-type-errors   #-}
module Organ where

import           Control.Newtype
import           Data.Char
import           Data.Functor.Contravariant
import           Data.Monoid
import           GHC.Generics
import           System.IO

type N m a = a -> m
type NN m a = N m (N m a)

shift ::a -> NN m a
shift x k = k x

unshift :: N m (NN m a) -> N m a
unshift k x = k (shift x)

data Source m a = Done | Cons a (Src m a) -- Cons a (Sink a -> m)
data Sink   m a = Full | Cont   (Snk m a) -- Cont (Source a -> m)

forward :: Monoid m => Source m a -> Sink m a -> m
forward s (Cont s')      = s' s
forward Done Full        = mempty
forward (Cons _ xs) Full = xs Full

instance Functor (Source m) where
  fmap _ Done       = Done
  fmap f (Cons a k) = Cons (f a) (k . contramap f)

instance Contravariant (Sink m) where
  contramap _ Full     = Full
  contramap f (Cont k) = Cont (k . fmap f)

-- | Builds a new 'Src' from the pull and abort continuations
newSrc :: ((a -> Src m a -> m) -> m -> m) -> m -> Src m a
newSrc _pull abort Full = abort
newSrc pull _abort (Cont k) = pull (\a src -> k $ Cons a src) (k Done)

-- breaks linearity ? I don't understand
pull :: Source m a -> m -> (a -> Source m a -> m) -> m
pull Done eof _      = eof
pull (Cons x cs) _ k = cs $ Cont $ \xs -> k x xs

push :: a -> Sink m a -> Src m a -> m
push x (Cont c) k = c (Cons x k)
push _ Full k     = k Full

drain :: Monoid m => Source m a -> Sink m a -> m
drain s (Cont s')     = s' s
drain Done Full       = mempty
drain (Cons _ k) Full = k Full

type Src m a = N m (Sink m a)   -- Sink m a -> m
type Snk m a = N m (Source m a) -- Source m a -> m

empty :: Monoid m => Src m a
empty = drain Done

plug :: Monoid m => Snk m a
plug source = drain source Full

mapSrc :: (a -> b) -> Src m a -> Src m b
mapSrc f = under Nly (fmap f)

mapSnk :: (a -> b) -> Snk m b -> Snk m a
mapSnk f = under Nly (contramap f)

onSource :: Monoid m => (Src m a -> t) -> Source m a -> t
onSource f s = f (s `drain`)

onSink :: Monoid m => (Snk m a -> t) -> Sink m a -> t
onSink f t = f (`drain` t)

shiftSrc :: Src m a -> N m (Snk m a)
shiftSrc k kk = k (Cont kk)

shiftSnk :: Snk m a -> N m (Src m a)
shiftSnk k kk = kk (Cont k)

fwd :: Src m a -> Snk m a -> m
fwd = shiftSrc

unshiftSrc :: Monoid m => N m (Snk m a) -> Src m a
unshiftSrc = onSink

unshiftSnk :: Monoid m => N m (Src m a) -> Snk m a
unshiftSnk = onSource

flipSrc :: Monoid m => (Src m b -> Src m a) -> Snk m a -> Snk m b
flipSrc f s = shiftSnk s . onSource f

flipSnk :: Monoid m => (Snk m a -> Snk m b) -> Src m b -> Src m a
flipSnk f s = shiftSrc s . onSink f

cons :: a -> Src m a -> Src m a
cons a s s' = push a s' s

tail :: Monoid m => Src m a -> Src m a
tail = flipSnk $ \t s -> case s of Done -> t Done ; Cons _ xs -> fwd xs t

each :: (Monoid m, Foldable t) => t a -> Src m a
each = foldr cons empty

filterSrc :: Monoid m => (a -> Bool) -> Src m a -> Src m a
filterSrc pred = flipSnk (filterSnk pred)

filterSnk :: Monoid m => (a -> Bool) -> Snk m a -> Snk m a
filterSnk _ snk Done = snk Done
filterSnk pred snk (Cons a k)
  | pred a = snk (Cons a (filterSrc pred k))
  | otherwise = k $ Cont $ filterSnk pred snk

takeSrc, dropSrc :: Monoid m => Int -> Src m a -> Src m a
takeSrc n = flipSnk (takeSnk n)
dropSrc n = flipSnk (dropSnk n)

takeSnk, dropSnk :: Monoid m => Int -> Snk m a -> Snk m a
dropSnk _ s Done         = s Done
dropSnk 0 s s'           = s s'
dropSnk n s (Cons _a xs) = shiftSrc (dropSrc (n-1) xs) s

takeSnk _ s Done        = s Done
takeSnk 0 s (Cons _ s') = s' Full <> s Done
takeSnk n s (Cons a s') = s $ Cons a $ takeSrc (n-1) s'

dropWhileSrc :: Monoid m => (a->Bool) -> Src m a -> Src m a
dropWhileSrc p = flipSnk(dropWhileSnk p)

dropWhileSnk :: Monoid m => (a->Bool) -> Snk m a -> Snk m a
dropWhileSnk _ s Done = s Done
dropWhileSnk p s s'@(Cons a xs)
  | p a = shiftSrc (dropWhileSrc p xs) s
  | otherwise = s s'

breakSrc :: Monoid m => (a -> Bool) -> Src m a -> Src m [a]
breakSrc f = flipSnk (unbreakSnk f)

unbreakSnk :: Monoid m => (a -> Bool) -> Snk m [a] -> Snk m a
unbreakSnk f = loop_snk [] where
  loop_snk []  s Done = s Done
  loop_snk acc s Done = s $ Cons (reverse acc) empty
  loop_snk acc s (Cons a xs)
    | f a = s $ Cons (reverse acc) (breakSrc f xs)
    | otherwise = xs $ Cont $ loop_snk (a:acc) s

linesSrc, wordsSrc :: Monoid m => Src m Char -> Src m String
linesSrc = breakSrc (=='\n')
wordsSrc = filterSrc isWord . breakSrc isSpace
  where
    isWord x = dropWhile isSpace x /= ""

wordsSnk :: Monoid m => Snk m String -> Snk m Char
wordsSnk = flipSrc wordsSrc

-- | Strict left fold
foldSrc :: Monoid m => (acc -> a -> acc) -> acc -> (acc -> b) -> Src m a -> NN m b
foldSrc f !z proj src nb = src $ Cont $ foldSnk f z proj nb

foldSnk :: Monoid m => (acc -> a -> acc) -> acc -> (acc -> b) -> N m b -> Snk m a
foldSnk _ !z proj nb  Done      = nb $ proj z
foldSnk f !z proj nb (Cons a s) = foldSrc f (f z a) proj s nb

toList :: Src m a -> NN m [a]
toList s k = shiftSrc s (toListSnk k)

toListSnk :: N m [a] -> Snk m a
toListSnk k Done       = k []
toListSnk k (Cons a s) = toList s (k . (a :))

-- would need difference lists, or reverse
-- toList2 = foldSrc (:) []

handleSrc :: Handle -> (Handle -> IO a) -> Src (IO()) a
handleSrc h _    Full = hClose h
handleSrc h get (Cont c) = do
  e <- hIsEOF h
  if e then do hClose h
               c Done
    else do x <- get h
            c (Cons x $ handleSrc h get)

fileSrc :: FilePath -> (Handle -> IO a) -> Src (IO()) a
fileSrc f get sink = do
  h <- openFile f ReadMode
  handleSrc h get sink

instance {-# OVERLAPPING #-} Monoid m => Monoid (Src m a) where
  mempty = empty
  mappend s1 s2 Full     = s1 Full <> s2 Full
  mappend s1 s2 (Cont s) = s1 $ Cont $ forwardThenSnk s s2

forwardThenSnk :: Monoid m => Snk m a -> Src m a -> Snk m a
forwardThenSnk snk src Done       = fwd src snk
forwardThenSnk snk src (Cons a s) = snk $ Cons a (s <> src)

-- | Also known as dup_oo
collapseSnk :: Monoid m => Snk m a -> Snk m a -> Snk m a
collapseSnk t1 t2 Done = t1 Done <> t2 Done
collapseSnk t1 t2 (Cons a s) =
  t1 $ Cons a $ \c1 ->
  t2 $ Cons a $ \c2 ->
    shiftSrc s $ collapseSnk (`drain` c1) (`drain` c2)

-- | Also known as dup_io
tee :: Monoid m => Src m a -> Snk m a -> Src m a
tee s t = flipSnk (collapseSnk t) s

zipSrc :: Monoid m => Src m t1 -> Src m t -> Src m (t1, t)
zipSrc s1 s2 t3 = shiftSrc s2 $ \s -> unshiftSrc (\t -> forkSnk t s1 s) t3

forkSnk :: Monoid m => Snk m (t1, t) -> Src m t1 -> Snk m t
forkSnk sab ta tb =
  shiftSrc ta $ \ta' ->
    case ta' of
      Done -> forward tb Full <> sab Done
      Cons a as -> case tb of
        Done      -> as Full <> sab Done
        Cons b bs -> fwd (cons (a, b) $ zipSrc as bs) sab

forkSrc :: Monoid t => Src t (a1, a) -> Snk t a1 -> Src t a
forkSrc sab ta tb = shiftSnk (zipSnk ta (`forward` tb)) sab

zipSnk :: Monoid t => Snk t a1 -> Snk t a -> Snk t (a1, a)
zipSnk sa sb Done = sa Done <> sb Done
zipSnk sa sb (Cons (a, b) tab) =
  sa $ Cons a $ \sa' -> sb $ Cons b $ \sb' -> forkSrc tab (`forward` sa') sb'

-- -------------------------------
-- Functors on function arguments
-- -------------------------------
newtype Nly b f a = Nly {nly :: f a -> b} deriving (Generic)
instance Newtype (Nly b f a)

instance Functor f => Contravariant (Nly m f) where
  contramap ba (Nly fa) = Nly (fa . fmap ba)

instance Contravariant f => Functor (Nly m f) where
  fmap f (Nly g) = Nly (g . contramap f)
