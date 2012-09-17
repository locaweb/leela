{-# LANGUAGE BangPatterns #-}
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

-- | A very simple asynchronous pure process.
module DarkMatter.Data.Proc
       ( Proc ()
       , Chunk(..)
       -- ^ Combinators
       , done
       , doneR
       , await
       , apply
       , pureF
       , pipe
       , window
       -- ^ Evaluators
       , feed
       , run
       , eval
       ) where

import           Prelude hiding (null, take, drop)
import qualified Data.List as L
import           Data.Monoid

data Proc i o = Put o i
              | Get (i -> Proc i o)

-- | Produces a value, usually meaning the proc is done
done :: (Monoid i) => o -> Proc i o
done = flip doneR mempty

-- | Produces a value with a leftover
doneR :: o -> i -> Proc i o
doneR = Put

-- | Requests more input
await :: (i -> Proc i o) -> Proc i o
await = Get

-- | Feeds a single input effectively moving the proc to its next
-- state.
feed :: (Monoid i) => Proc i o -> i -> Proc i o
feed (Get f) i    = f i
feed (Put a i0) i = Put a (i0 `mappend` i)

-- | Feed everything into the proc until no more input is
-- available. Notice the output may not be a 1:1 correspondence to the
-- input.
run :: (Monoid i, Chunk i) => Proc i o -> [i] -> [o]
run f = go f
  where go (Put o _) [] = [o]
        go _ []         = []
        go g (x:xs)     = case (feed g x)
                          of Put o i
                               | null i    -> o : go f xs
                               | otherwise -> o : go f (i:xs)
                             h             -> go h xs

-- | Evaluates the process. Right is used when the proc has produced a
-- result. Left when it is requesting more data.
eval :: Proc i o -> Either (Proc i o) (o, i)
eval (Put o i) = Right (o, i)
eval f         = Left f

-- | Apply a pure function over the proc.
apply :: (a -> b) -> Proc i a -> Proc i b
apply f (Put a i) = doneR (f a) i
apply f (Get g)   = await (\i -> apply f (g i))

-- | Connects two procs. It fully evaluatates the first proc and its
-- result is fed up into the second proc. Notice that if the second
-- proc does not requests input, the first one is completely ignored.
pipe :: (Monoid a, Chunk a) => Proc a a  -- ^ The first proc
                            -> Proc a a  -- ^ The second proc
                            -> Proc a a
pipe (Put _ i0) (Put a i1) = Put a (i0 `mappend` i1)
pipe (Put a i0) (Get f)
  | null i0                = f a
  | otherwise              = addResidue i0 $ f a
pipe (Get f) g
  | isPut g                = g
  | otherwise              = await (\i -> f i `pipe` g)

-- | Uses a pure function as a proc.
pureF :: (Monoid i) => (i -> o) -> Proc i o
pureF f = await (done . f)

-- | TODO:fixme
window :: (Monoid i, Chunk i, ChunkListLike i) => Int -> Int -> Proc i i
window n m = go mempty
  where go !acc
          | size acc >= n = if (n == m)
                            then uncurry doneR (split n acc)
                            else doneR (take n acc) (drop m acc)
          | otherwise     = await (\i -> go (acc `mappend` i))

-- mapR :: (a -> [a] -> b) -> [a] -> [b]
-- mapR _ []     = []
-- mapR f (x:xs) = f x xs : mapR f xs

addResidue :: (Monoid i) => i -> Proc i o -> Proc i o
addResidue i (Put a i1) = Put a (i `mappend` i1)
addResidue i (Get f)    = Get (\i1 -> addResidue i $ f i1)

isPut :: Proc i o -> Bool
isPut (Put _ _) = True
isPut _         = False

class Chunk i where
  
  null :: i -> Bool

class ChunkListLike i where

  size  :: i -> Int

  take  :: Int -> i -> i
  take n = fst . split n

  drop  :: Int -> i -> i
  drop n = snd . split n

  split :: Int -> i -> (i, i)
  split n i = (take n i, drop n i)

instance Functor (Proc i) where

  fmap = apply

instance Chunk [a] where
  
  null (_:_) = False
  null _     = True

instance ChunkListLike [a] where

  size  = L.length

  take  = L.take

  drop  = L.drop

  split = L.splitAt
