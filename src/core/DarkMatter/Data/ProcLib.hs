-- All Rights Reserved.
--
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
--
--        http://www.apache.org/licenses/LICENSE-2.0
--
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.

-- | Provides a series of numeric functions and combinators useful to
-- this project.
module DarkMatter.Data.ProcLib where

import DarkMatter.Data.Proc

-- | Computes the mean using an numerically stable algorithm:
-- @
--    meanₙ₊₁ = meanₙ + aₖ₊₁ - meanₙ
--                      ――――――――――――
--                          n+1
-- @
-- 
-- One possible way to verify the above equation is correct is solving
-- the following on x:
-- @
--     ₙ            ₙ₊₁
--    ∑ aₖ         ∑ aₖ
--     ₖ₌₁          ₖ₌₁
--   ―――――― + x = ――――――
--      n          n+1
-- @ 
mean :: (Fractional a) => Proc a a
mean = Proc (f 0 1)
  where f m0 n e = let m1 = m0 + (e - m0)/n
                   in (m1, Proc (f m1 (n + 1)))

-- | Simple moving average
sma :: (Fractional a) => Int -> Proc a a
sma n = Proc (f [])
  where f acc i
          | length acc == n = g acc i
          | otherwise       = (0, Proc $ f (i : acc))

        g acc i = let m = fst $ run_ mean acc
                  in (m, Proc $ g (i : init acc))

-- | Drops the first n items
dropProc :: Int -> Proc a (Maybe a)
dropProc 0 = Proc $ \i -> (Just i, dropProc 0)
dropProc n = Proc $ const (Nothing, dropProc (n-1))

-- | Takes the first n items
takeProc :: Int -> Proc a (Maybe a)
takeProc 0 = Proc $ const (Nothing, takeProc 0)
takeProc n = Proc $ \i -> (Just i, takeProc (n-1))

-- | Exponential moving average
ema :: (Fractional a) => a -> Proc a a
ema a = Proc (f 0)
  where f m0 i = let m = m0 + a * (i - m0)
                 in (m, Proc (f m))

-- | Count how many items this proc has seen
count :: (Integral b) => Proc a b
count = Proc (f 0)
  where f n _ = (n + 1, Proc $ f (n + 1))

-- | Uses an binary function to create a proc. Possible uses:
-- @
--   binary (+)
--   binary (-)
--   binary (max)
-- @
binary :: (a -> a -> a) -> Proc a a
binary f = Proc (\i -> (i, Proc $ g i))
  where g a b = (f a b, Proc (g $ f a b))

-- | Uses a predicate to decide whether or not to return an
-- element. When this predicate evaluates to false, Nothing is
-- returned
select :: (a -> Bool) -> Proc a (Maybe a)
select p = Proc f
  where f i
          | p i       = (Just i, select p)
          | otherwise = (Nothing, select p)

-- | Sample @n@ elements out from a population of @m@ elements. For
-- every value, @n@ and @m@ are decremented. The function returns
-- @Nothing@ if @n == 0@ and the process starts over with the original
-- values when @m == 0@
sample :: Int  -- ^ Number of elements to select (@n@)
       -> Int  -- ^ Population size (@m@)
       -> Proc i (Maybe i)
sample n0 m0 = Proc (f n0 m0)
  where f _ 0 a       = (Just a, Proc $ f (n0-1) (m0-1))
        f n m a
          | n > 0     = (Just a, Proc $ f (n - 1) (m - 1))
          | otherwise = (Nothing, Proc $ f 0 (m - 1))

-- | Keeps an internal buffer of n elements using a proc to process it
-- when it is full. Possible use @window 30 mean@
window :: Int              -- ^ The number of elements to buffer
       -> Proc i o         -- ^ The proc to execute when the buffer is full
       -> Proc i (Maybe o) -- ^ The resulting `window proc'
window n f = Proc (go (0,[]))
  where go (k,acc) i
          | k + 1 == n = let (o, _) = run_ f (i : acc)
                         in (Just o, window n f)
          | otherwise  = (Nothing, Proc $ go (k + 1, i : acc))

