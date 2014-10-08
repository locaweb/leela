{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Leela.Data.Funclib
       ( Func (dump, load, invoke)
       , func
       , func_
       , run
       , run_
       , iter
       , pairV
       , pairVV
       , liftMap1
       , liftMapV
       , liftFold1
       , liftFoldV
       , liftFilter
       , ewma
       , mean
       , hmean
       , count1
       , countV
       , window
       , windowBy
       ) where

import           Data.List
import qualified Data.Vector as V

data Func s a b = Func { dump    :: s
                       , load    :: s -> Func s a b
                       , invoke  :: s -> a -> (Func s a b, b)
                       }

instance Functor (Func s a) where

  fmap f (Func s _ g) = func s $ \s' a -> let (h, b) = g s' a in (fmap f h, f b)

pipe :: Func f a b -> Func g b c -> Func (Func f a b, Func g b c) a c
pipe f0 g0 = func (f0, g0) exec
    where
      exec (f, g) a = let (f1, b) = run f a
                          (g1, c) = run g b
                      in (pipe f1 g1, c)

par :: Func f a b -> Func g a c -> Func (Func f a b, Func g a c) a (b, c)
par f0 g0 = func (f0, g0) exec
    where
      exec (f, g) a = let (f1, b) = run f a
                          (g1, c) = run g a
                      in (par f1 g1, (b, c))

func_ :: (a -> (Func () a b, b)) -> Func () a b
func_ f = func () $ \_ -> f

func :: s -> (s -> a -> (Func s a b, b)) -> Func s a b
func s f = Func s (flip func f) $ \s' a -> let (g, b) = f s' a in b `seq` (g, b)

run :: Func s a b -> a -> (Func s a b, b)
run (Func s _ f) = f s

run_ :: Func s a b -> a -> b
run_ f = snd . run f

runMap :: Func s a b -> [a] -> (Func s a b, [b])
runMap = mapAccumL run

runMap_ :: Func s a b -> [a] -> [b]
runMap_ f = snd . runMap f

liftFoldV :: (b -> a -> b) -> (a -> b) -> Func (Maybe b) (V.Vector a) b
liftFoldV f g = func Nothing next
    where
      next Nothing v  = next (Just (g $ V.head v)) (V.tail v)

      next (Just z) a = let b = V.foldl' f z a
                        in (func (Just b) next, b)

liftFold1 :: (b -> a -> b) -> (a -> b) -> Func (Maybe b) a b
liftFold1 f g = func Nothing next
    where
      next Nothing a = let b = g a
                       in (func (Just b) next, b)

      next (Just z) a = let b = f z a
                        in (func (Just b) next, b)

liftFilter :: (a -> Bool) -> Func () (V.Vector a) (V.Vector a)
liftFilter f = func_ (\a -> (liftFilter f, V.filter f a))

liftMapV :: (a -> b) -> Func () (V.Vector a) (V.Vector b)
liftMapV f = func_ (\a -> (liftMapV f, V.map f a))

liftMap1 :: (a -> b) -> Func () a b
liftMap1 f = func_ (\a -> (liftMap1 f, f a))

identity :: Func () a a
identity = func_ (\a -> (identity, a))

constFun :: a -> Func () a a
constFun a = func_ (\_ -> (constFun a, a))

mean :: Fractional a => Func (Maybe (a, a)) (V.Vector a) a
mean = func Nothing next
    where
      next Nothing as        = let m = V.head as
                               in next (Just (m, 1)) (V.tail as)
      next (Just (m0, n)) as = let m  = fromIntegral $ V.length as
                                   m1 = m0 + (V.foldl' (\s a -> a - m0 + s) 0 as) / (n + m)
                               in (func (Just (m1, n + m)) next, m1)

hmean :: Fractional a => Func (Maybe (a, a)) (V.Vector a) a
hmean = func (dump mean) next
    where
      next s as = let (f, v) = run (load mean s) (V.map recip as)
                  in (func (dump f) next, recip v)

ewma :: Num a => a -> Func (Maybe a) (V.Vector a) a
ewma alpha = func Nothing next
    where
      next Nothing as   = next (Just $ V.head as) (V.tail as)

      next (Just m0) as = let m1 = V.foldl' (\m a -> m + alpha * (a - m)) m0 as
                          in (func (Just m1) next, m1)

countV :: Num a => Func a (V.Vector b) a
countV = func 0 next
    where
      next a v = let b = a + (fromIntegral $ V.length v)
                 in (func b next, b)

iter :: Func s (V.Vector a) b -> Func s (V.Vector a) (V.Vector b)
iter f = func (dump f) next
    where
      next s xs = let (g, ys) = runMap (load f s) (V.toList $ V.map V.singleton xs)
                  in (func (dump g) next, V.fromList ys)

count1 :: Num a => Func (Maybe a) b a
count1 = liftFold1 (\s _ -> s + 1) (const 1)

window :: Int -> Func s (V.Vector a) b -> Func (V.Vector a) (V.Vector a) (V.Vector b)
window n f = func V.empty buffer
    where
      buffer xs ys
        | V.length as >= n = let (v, r) = flush [] n as
                             in (func r buffer, v)
        | otherwise        = (func as buffer, V.empty)
          where
            as = xs V.++ ys

      flush acc k as
        | V.length as < k = (V.fromList $ reverse acc, as)
        | otherwise       = let (xs, ys) = V.splitAt k as
                            in flush (run_ f xs : acc) k ys

windowBy :: (a -> a -> Bool) -> Func s (V.Vector a) b -> Func (V.Vector a) (V.Vector a) (V.Vector b)
windowBy p f = func V.empty buffer
    where
      t0 = V.head
      t1 = V.last

      buffer xs ys
        | p (t0 as) (t1 as) = let (v, r) = flush [] as
                              in (func r buffer, v)
        | otherwise         = (func as buffer, V.empty)
          where
            as = xs V.++ ys

      flush acc as
        | V.null as         = (V.fromList $ reverse acc, V.empty)
        | p (t0 as) (t1 as) = let (xs, r) = V.break (p (t0 as)) as
                              in flush (run_ f xs : acc) r
        | otherwise         = (V.fromList $ reverse acc, as)

pairV :: Func s (V.Vector a) b -> Func s (V.Vector (t, a)) (t, b)
pairV f = func (dump f) go
    where
      go s a = let (h, b) = run (load f s) (V.map snd a)
               in (func (dump h) go, (fst $ V.head a, b))

pairVV :: Func s (V.Vector a) (V.Vector b) -> Func s (V.Vector (t, a)) (V.Vector (t, b))
pairVV f = func (dump f) go
    where
      go s v = let (vt, va) = V.unzip v
                   (g, vb)  = run (load f s) va
               in (func (dump g) go, V.zip vt vb)
