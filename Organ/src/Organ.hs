{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Organ where

import           Control.Lens        hiding (cons)
import           Control.Monad.Catch
import           Data.Char
import           Data.Functor.Nly
import           Data.Monoid
import           System.IO

type N m a = a -> m
type NN m a = N m (N m a)

shift ::a -> NN m a
shift x k = k x

unshift :: N m (NN m a) -> N m a
unshift k x = k (shift x)

data Source m a = Done (Maybe SomeException) | Cons a (Src m a) -- Cons a (Sink a -> m)
data Sink   m a = Full (Maybe SomeException) | Cont   (Snk m a) -- Cont (Source a -> m)

forward :: Monoid m => Source m a -> Sink m a -> m
forward s (Cont s')          = s' s
forward Done{} Full{}        = mempty
forward (Cons _ xs) x@Full{} = xs x

instance Functor (Source m) where
  fmap _ (Done e)   = Done e
  fmap f (Cons a k) = Cons (f a) (k . contramap f)

instance Contravariant (Sink m) where
  contramap _ (Full e) = Full e
  contramap f (Cont k) = Cont (k . fmap f)

-- | Builds a new 'Src' from the pull and abort continuations
newSrc :: ((a -> Src m a -> m) -> (Maybe SomeException -> m) -> m) -> (Maybe SomeException -> m) -> Src m a
newSrc _pull abort (Full e) = abort e
newSrc pull _abort (Cont k) = pull (\a src -> k $ Cons a src) (k . Done)

-- breaks linearity ? I don't understand
pull :: Source m a -> (Maybe SomeException -> m) -> (a -> Source m a -> m) -> m
pull (Done e) done _ = done e
pull (Cons x cs) _ k = cs $ Cont $ \xs -> k x xs

push :: a -> Sink m a -> Src m a -> m
push x (Cont c) k = c (Cons x k)
push _ x@Full{} k = k x

drain :: Monoid m => Source m a -> Sink m a -> m
drain s (Cont s')         = s' s
drain Done{} Full{}       = mempty
drain (Cons _ k) x@Full{} = k x

type Src m a = N m (Sink m a)   -- Sink m a -> m
type Snk m a = N m (Source m a) -- Source m a -> m

empty :: Monoid m => Src m a
empty = drain (Done Nothing)

plug :: Monoid m => Snk m a
plug source = drain source (Full Nothing)

mapSrc :: (a -> b) -> Src m a -> Src m b
mapSrc f = under _Nly (fmap f)

mapSrcM :: MonadCatch m => (a -> m b) -> Src (m()) a -> Src (m()) b
mapSrcM _ src (Full e) = src (Full e)
mapSrcM f src (Cont k) = src $ Cont $ \case
  Done e -> k $ Done e
  Cons a rest -> do
    b <- try $ f a
    case b of
      Right b -> k (Cons b $ mapSrcM f rest)
      Left e  -> k (Done (Just e)) *> throwM e

mapSnk :: (a -> b) -> Snk m b -> Snk m a
mapSnk f = under _Nly (contramap f)

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
tail = flipSnk $ \t s -> case s of x@Done{} -> t x ; Cons _ xs -> fwd xs t

each :: (Monoid m, Foldable t) => t a -> Src m a
each = foldr cons empty

filterSrc :: Monoid m => (a -> Bool) -> Src m a -> Src m a
filterSrc pred = flipSnk (filterSnk pred)

filterSnk :: Monoid m => (a -> Bool) -> Snk m a -> Snk m a
filterSnk _ snk x@Done{} = snk x
filterSnk pred snk (Cons a k)
  | pred a = snk (Cons a (filterSrc pred k))
  | otherwise = k $ Cont $ filterSnk pred snk

takeSrc, dropSrc :: Monoid m => Int -> Src m a -> Src m a
takeSrc n = flipSnk (takeSnk n)
dropSrc n = flipSnk (dropSnk n)

takeSnk, dropSnk :: Monoid m => Int -> Snk m a -> Snk m a
dropSnk _ s x@Done{}     = s x
dropSnk 0 s s'           = s s'
dropSnk n s (Cons _a xs) = shiftSrc (dropSrc (n-1) xs) s

takeSnk _ s x@Done{}    = s x
takeSnk 0 s (Cons _ s') = s' (Full Nothing) <> s (Done Nothing)
takeSnk n s (Cons a s') = s $ Cons a $ takeSrc (n-1) s'

dropWhileSrc :: Monoid m => (a->Bool) -> Src m a -> Src m a
dropWhileSrc p = flipSnk(dropWhileSnk p)

dropWhileSnk :: Monoid m => (a->Bool) -> Snk m a -> Snk m a
dropWhileSnk _ s x@Done{} = s x
dropWhileSnk p s s'@(Cons a xs)
  | p a = shiftSrc (dropWhileSrc p xs) s
  | otherwise = s s'

breakSrc :: Monoid m => (a -> Bool) -> Src m a -> Src m [a]
breakSrc f = flipSnk (unbreakSnk f)

unbreakSnk :: Monoid m => (a -> Bool) -> Snk m [a] -> Snk m a
unbreakSnk f = loop_snk [] where
  loop_snk []  s (Done e) = s $ Done e
  loop_snk acc s (Done Nothing) = s $ Cons (reverse acc) empty
  loop_snk _   s (Done e@Just{}) = s (Done e)
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
foldSrc
  :: (Monoid (m()), MonadThrow m)
  => (acc -> a -> acc) -> acc -> (acc -> b) -> Src (m ()) a -> NN (m ()) b
foldSrc f !z proj src nb = src $ Cont $ foldSnk f z proj nb

foldSnk
  :: (MonadThrow m, Monoid (m()))
  => (acc -> a -> acc) -> acc -> (acc -> b) -> N (m ()) b -> Snk (m()) a
foldSnk _ !z proj nb (Done Nothing)  = nb $ proj z
foldSnk _ !_ _    _  (Done (Just e)) = throwM e
foldSnk f !z proj nb (Cons a s)      = foldSrc f (f z a) proj s nb

toList :: MonadThrow m => Src (m()) a -> NN (m()) [a]
toList s k = shiftSrc s (toListSnk k)

toListSnk :: MonadThrow m => N (m()) [a] -> Snk (m()) a
toListSnk _ (Done (Just e)) = throwM e
toListSnk k (Done Nothing)  = k []
toListSnk k (Cons a s)      = toList s (k . (a :))

-- would need difference lists, or reverse
-- toList2 = foldSrc (:) []

handleSrc :: Handle -> (Handle -> IO a) -> Src (IO()) a
handleSrc h _   (Full e) = hClose h *> maybe (return()) throwM e
handleSrc h get (Cont c) = handle (c . Done . Just) $ do
  e <- hIsEOF h
  if e then do hClose h
               c (Done Nothing)
    else do x <- get h
            c (Cons x $ handleSrc h get)

fileSrc :: FilePath -> (Handle -> IO a) -> Src (IO()) a
fileSrc f get sink = do
  h <- openFile f ReadMode
  handleSrc h get sink

instance {-# OVERLAPPING #-} Monoid m => Monoid (Src m a) where
  mempty = empty
  mappend s1 s2 x@Full{} = s1 x <> s2 x
  mappend s1 s2 (Cont s) = s1 $ Cont $ forwardThenSnk s s2

forwardThenSnk :: Monoid m => Snk m a -> Src m a -> Snk m a
forwardThenSnk snk src (Done Nothing)  = fwd src snk
forwardThenSnk snk _   x@(Done Just{}) = snk x
forwardThenSnk snk src (Cons a s)      = snk $ Cons a (s <> src)

-- | Also known as dup_oo
collapseSnk :: Monoid m => Snk m a -> Snk m a -> Snk m a
collapseSnk t1 t2 x@Done{} = t1 x <> t2 x
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
      Done e        -> forward tb (Full e) <> sab (Done e)
      Cons a as -> case tb of
        Done e    -> as (Full e) <> sab (Done e)
        Cons b bs -> fwd (cons (a, b) $ zipSrc as bs) sab

forkSrc :: Monoid t => Src t (a1, a) -> Snk t a1 -> Src t a
forkSrc sab ta tb = shiftSnk (zipSnk ta (`forward` tb)) sab

zipSnk :: Monoid t => Snk t a1 -> Snk t a -> Snk t (a1, a)
zipSnk sa sb (Done Nothing) = sa (Done Nothing) <> sb (Done Nothing)
zipSnk sa sb (Done e) = sa (Done e) <> sb (Done e)
zipSnk sa sb (Cons (a, b) tab) =
  sa $ Cons a $ \sa' -> sb $ Cons b $ \sb' -> forkSrc tab (`forward` sa') sb'

