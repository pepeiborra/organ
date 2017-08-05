{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Organ
  (
  -- * Negations
    N, NN
  , shift, unshift
  -- * Sources and Sinks
  , Source(..), Sink(..)
  , Src, Snk
  -- ** Constructors
  , empty, plug
  , newSrc
  , handleSrc
  , fileSrc
  , Organ.each
  -- ** Execution
  , toListSrc, toListSnk
  , forward, fwd
  , drain
  -- ** Operations
  , pull, push
  , mapSrc, mapSnk
  , traverseSrc, traverseSnk
  , onSource, onSink
  , shiftSrc, shiftSnk
  , unshiftSrc, unshiftSnk
  , flipSrc, flipSnk
  , cons, Organ.tail
  , filterSrc, filterSnk
  , takeSrc, takeSnk
  , dropSrc, dropSnk
  , dropWhileSrc, dropWhileSnk
  , breakSrc, unbreakSnk
  , linesSrc
  , wordsSrc, wordsSnk
  , foldSrc, foldSnk
  , collapseSnk
  , tee
  , zipSrc, zipSnk
  , forkSrc, forkSnk
  , dupSrc
  , groupSrc
  , mergeSrc
  , chunkSrc
  , unchunkSrc
  , dmux
  , CoSrc, CoSnk
  , mapCoSrc, mapCoSnk
  , mux
  , Schedule, scheduleSrc, scheduleSnk
  , sequentially, concurrently
  )where

import           Control.Concurrent
import           Control.Lens           hiding (cons, each)
import           Control.Lens.Extras
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Functor.Nly
import           Data.List.NonEmpty     (NonEmpty (..), nonEmpty)
import           Data.Monoid
import qualified Data.Semigroup         as S
import           System.IO

type N m a = a -> m
type NN m a = N m (N m a)

shift ::a -> NN m a
shift x k = k x

unshift :: N m (NN m a) -> N m a
unshift k x = k (shift x)

data Source m a = Done (Maybe SomeException) | Cons a (Src m a)
data Sink   m a = Full (Maybe SomeException) | Cont   (Snk m a) -- Cont (Source a -> m)

type Src m a = N m (Sink m a)   -- Sink m a -> m
type Snk m a = N m (Source m a) -- Source m a -> m

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

empty :: Monoid m => Src m a
empty = drain (Done Nothing)

plug :: Monoid m => Snk m a
plug source = drain source (Full Nothing)

mapSrc :: (a -> b) -> Src m a -> Src m b
mapSrc f = under _Nly (fmap f)

traverseSrc :: MonadCatch m => (a -> m b) -> Src (m()) a -> Src (m()) b
traverseSrc _ src (Full e) = src $ Full e
traverseSrc f src (Cont k) = src $ Cont $ traverseSnk f k

traverseSnk :: MonadCatch m => (b -> m a) -> Snk (m()) a -> Snk (m()) b
traverseSnk _ snk (Done e)    = snk (Done e)
traverseSnk f snk (Cons x xs) = do
  b <- try $ f x
  case b of
    Right b -> snk (Cons b $ traverseSrc f xs)
    Left e -> snk (Done (Just e)) *> throwM e

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
unbreakSnk f s = loop_snk [] where
  loop_snk []  (Done e) = s $ Done e
  loop_snk acc (Done Nothing) = s $ Cons (reverse acc) empty
  loop_snk _   (Done e@Just{}) = s (Done e)
  loop_snk acc (Cons a xs)
    | f a = s $ Cons (reverse acc) (breakSrc f xs)
    | otherwise = xs $ Cont $ loop_snk (a:acc)

chunkSrc :: Monoid m => Int -> Src m a -> Src m [a]
chunkSrc n = flipSnk (unsplitSnk n)

unsplitSnk :: Monoid m => Int -> Snk m [a] -> Snk m a
unsplitSnk 0 _ = error "cannot split in chunks of 0 elements"
unsplitSnk n s = loop_unsplit (0,id) where
  loop_unsplit (0,_)  (Done e) = s $ Done e
  loop_unsplit (_,acc) (Done Nothing) = s $ Cons (acc []) mempty
  loop_unsplit _ (Done e) = s (Done e)
  loop_unsplit (i,acc) (Cons x xs)
    | i == n - 1 = s $ Cons (acc [x]) (chunkSrc n xs)
    | otherwise = xs $ Cont $ loop_unsplit (i+1, acc . (x:))

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

toListSrc :: MonadThrow m => Src (m()) a -> NN (m()) [a]
toListSrc s k = shiftSrc s (toListSnk k)

toListSnk :: MonadThrow m => N (m()) [a] -> Snk (m()) a
toListSnk _ (Done (Just e)) = throwM e
toListSnk k (Done Nothing)  = k []
toListSnk k (Cons a s)      = toListSrc s (k . (a :))

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
fileSrc f get snk = do
  h <- openFile f ReadMode
  handleSrc h get snk

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

data DupState m a = DupState
  { aborted :: Maybe (Either () ())
  , buffer  :: Maybe (Either (NonEmpty a) (NonEmpty a))
  , parent  :: Src m a
  }

dupSrc
  :: forall m a .
  (MonadIO m)
  => Src (m ()) a -> m (Src (m ()) a, Src (m ()) a)
dupSrc src = do
  state <- liftIO $ newMVar $ DupState Nothing Nothing src
  let run :: (forall b. Prism' (Either b b) b) -> Src (m()) a
      run me full@Full{} = do
        s@DupState{..} <- liftIO $ takeMVar state
        case aborted of
          Just x | is me x ->
            -- Nothing to do, me has already been aborted
            liftIO $ putMVar state s
          Just _ -> do
            -- The other one has been aborted, and now me, so abort the parent
            liftIO $ putMVar state s
            parent full
          Nothing -> do
            let buffer'
                  | Just x <- buffer
                  , is me x = Nothing
                  | otherwise = buffer
            liftIO $ putMVar state DupState{ aborted = Just (review me ())
                                           , buffer = buffer'
                                           , ..}
      run me (Cont k) = do
        s@DupState{..} <- liftIO $ takeMVar state
        case buffer of
          -- Catch up with other src
          Just (preview me -> Just (x :| rest)) -> do
            liftIO $ putMVar state $ s{buffer = fmap (review me) (nonEmpty rest)}
            k $ Cons x (run me)
          _ -> -- Consume and buffer
            parent $ Cont $
              \case
                x@Done{} -> do
                  liftIO $ putMVar state s
                  k x
                Cons a parent' -> do
                  let buffer'
                       | Just _ <- aborted = buffer
                       | Just b <- buffer = Just $ b & me %~ ([a] S.<>)
                       | otherwise = Just (review me [a])
                  liftIO $ putMVar state s{buffer = buffer', parent = parent'}
                  k $ Cons a (run me)
  return (run _Left, run _Right)

-- | Remove consecutive duplicates
groupSrc :: (Eq a, Monoid m) => Src m a -> Src m a
groupSrc = flipSnk groupSnk

-- | Remove consecutive duplicates
groupSnk :: forall a m . (Eq a, Monoid m) => Snk m a -> Snk m a
groupSnk = go Nothing where
  goSrc x = flipSnk (go x)
  go :: Maybe a -> Snk m a -> Snk m a
  go _ s x@Done{} = s x
  go Nothing  s (Cons x xs) = s $ Cons x $ flipSnk (go (Just x)) xs
  go (Just x) s (Cons x' xs)
    | x == x'   = shiftSrc (goSrc (Just x) xs) s
    | otherwise = go Nothing s (Cons x' xs)

-- | Sorted merge of two sources
mergeSrc :: Ord a => Src m a -> Src m a -> Src m a
mergeSrc s1 s2 = undefined s1 s2

unchunkSrc :: (Foldable t, Monoid m) => Src m (t a) -> Src m a
unchunkSrc = flipSnk chunkSnk

chunkSnk :: (Monoid m, Foldable t)=> Snk m a -> Snk m (t a)
chunkSnk s (Done e) = s (Done e)
chunkSnk s (Cons x xs) = fwd (each x <> unchunkSrc xs) s

dmux :: Monoid m => Src m (Either a b) -> Snk m a -> Snk m b -> m
dmux sab ta tb = shiftSnk ta $ \ta' -> shiftSnk tb $ \tb' -> shiftSrc sab $ \sab' -> dmux' sab' ta' tb'

dmux' :: Monoid m => Source m (Either a b) -> Sink m a -> Sink m b -> m
dmux' (Done e) ta tb = forward (Done e) ta <> forward (Done e) tb
dmux' (Cons (Left a) rest) ta tb = rest $ Cont $ \src' ->
  case ta of
    Full e -> forward (Done Nothing) tb <> drain src' (Full e)
    Cont k -> k $ Cons a $ \ta' -> dmux' src' ta' tb
dmux' (Cons (Right b) rest) ta tb = rest $ Cont $ \src' ->
  case tb of
    Full e -> forward (Done Nothing) ta <> drain src' (Full e)
    Cont k -> k $ Cons b $ \tb' -> dmux' src' ta tb'

-- ------------------------------------------------------------

type CoSrc m a = Snk m (N m a)
type CoSnk m a = Src m (N m a)

type Neither m a b = N m (Either (N m a) (N m b))

nnElimSrc :: Monoid m => Src m (NN m a) -> Src m a
nnElimSrc = flipSnk nnIntroSnk

nnElimSnk :: Monoid m => Snk m (NN m a) -> Snk m a
nnElimSnk = mapSnk shift

nnIntroSnk :: Monoid m => Snk m a -> Snk m (NN m a)
nnIntroSnk k (Done e) = k (Done e)
nnIntroSnk k (Cons x src) = x $ \x' -> k $ Cons x' (nnElimSrc src)

mux :: Monoid m => CoSrc m a -> CoSrc m b -> CoSrc m (Neither m a b)
mux sa sb = unshiftSnk $ \tab -> dmux (nnElimSrc tab) sa sb

mapCoSrc :: Monoid m => (a -> b) -> CoSrc m a -> CoSrc m b
mapCoSrc f = mapSnk (\ k a -> k $ f a)

mapCoSnk :: Monoid m => (b -> a) -> CoSnk m a -> CoSnk m b
mapCoSnk f = mapSrc (\ k a -> k $ f a)

type Schedule m = forall a. Source m a -> Source m (N m a) -> m

scheduleSrc :: Schedule m -> Src m a -> CoSrc m a
scheduleSrc sched src sink = shiftSrc src $ \source -> sched source sink

scheduleSnk :: Schedule m -> CoSnk m a -> Snk m a
scheduleSnk sched src sink = shiftSrc src $ \source -> sched sink source

sequentially :: Monoid m => Schedule m
sequentially (Done e)    (Cons _ k)  = k (Full e)
sequentially (Cons _ k)  (Done e)    = k (Full e)
sequentially Done{}      Done{}      = mempty
sequentially (Cons a aa) (Cons ka kaa) = ka a <> shiftSrc aa (\aa -> shiftSrc kaa $ \kaa -> sequentially aa kaa)

concurrently :: Schedule (IO ())
concurrently (Cons a aa) (Cons ka kaa) = void(forkIO(ka a)) <> shiftSrc aa (\aa -> shiftSrc kaa $ \kaa -> concurrently aa kaa)
concurrently a b = sequentially a b
