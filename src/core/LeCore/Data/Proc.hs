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
module LeCore.Data.Proc
       ( Proc ()
       , Chunk (..)
       -- ^ Combinators
       , done
       , await
       , apply
       -- ^ Evaluators
       , run
       , runC
       , eval
       -- ^ Connectors
       , pipe
       -- ^ Utility procs
       , pure
       , window
       , pureC
       , windowC
       -- ^ Chunk related
       , fromList
       , toList
       ) where

import Data.Monoid

data Proc i o = Put o
              | Get (i -> Proc i o)

data Chunk i = Chunk { val :: i
                     , eof :: Bool
                     }

-- | Produces a value, usually meaning the proc is done
done :: o -> Proc i o
done = Put

-- | Requests more input
await :: (i -> Proc i o) -> Proc i o
await = Get

suffixR :: (a -> [a] -> b) -> [a] -> [b]
suffixR _ []     = []
suffixR f (x:xs) = f x xs : suffixR f xs

fromList :: [a] -> [Chunk a]
fromList = suffixR (\c -> Chunk c . null)

toList :: [Chunk a] -> [a]
toList = map val

-- | Feed everything into the proc until no more input is
-- available. Notice the output may not be a 1:1 correspondence in
-- to the input.
run :: Proc i o -> [i] -> [o]
run f = go f f
  where go _ _ []     = []
        go z g (x:xs) = case (eval g x)
                        of Right o  -> o : go z z xs
                           Left h   -> go z h xs

-- | Same as run, but transforms the input into chunks so the proc
-- knows when it is EOF.
runC :: Proc (Chunk i) o -> [i] -> [o]
runC f = run f . fromList

-- | Evaluates a single input. Right is used when the process has
-- produced a value, with a possibly leftover. Left is returned when
-- the process is requesting more input.
eval :: Proc i o -> i -> Either (Proc i o) o
eval (Put o) _ = Right o
eval (Get f) i = case f i
                  of Put o -> Right o
                     h     -> Left h

-- | Apply a pure function over the proc.
apply :: (a -> b) -> Proc i a -> Proc i b
apply f (Put a) = done (f a)
apply f (Get g) = await (\i -> apply f (g i))

-- | Connects two procs. It fully evaluatates the first proc and its
-- result is fed up into the second proc. Notice that if the second
-- proc does not requests input, the first one is completely ignored.
pipe :: Proc a a  -- ^ The first proc
     -> Proc a a  -- ^ The second proc
     -> Proc a a
pipe _ (Put a)          = done a
pipe (Put a) (Get f)    = f a
pipe (Get f) g0@(Get g) = await (\i -> case (f i)
                                         of Put a -> g a
                                            h     -> h `pipe` g0)

-- | Uses a pure function as a proc.
pure :: (i -> o) -> Proc i o
pure f = await (done . f)

-- | Same as pure but works with Chunk.
pureC :: (i -> o) -> Proc (Chunk i) o
pureC f = pure (f . val)

-- | Apply a function of a group of n items. This is not enforced, as
-- an EOF value may force it to use less than n elements.
windowC :: Int         -- ^ How many items to group
       -> ([i] -> o)   -- ^ The function to use to produce a value
       -> Proc (Chunk i) o
windowC n f = go []
  where go !acc
          | length acc == n = done (f acc)
          | otherwise       = await (\i -> let v = val i
                                           in if (eof i)
                                              then done (f $ v : acc)
                                              else go (v:acc))

-- | Same as window, but always enforce n elements.
window :: Int         -- ^ How many items to group
        -> ([i] -> o)  -- ^ The function to use to produce a value
        -> Proc i o
window n f = go []
  where go !acc
          | length acc == n = done (f acc)
          | otherwise       = await (\i -> go (i:acc))

instance Functor (Proc i) where

  fmap = apply

instance Functor Chunk where

  fmap f c = c { val = f (val c) }

instance (Monoid i) => Monoid (Chunk i) where

  mempty      = Chunk mempty False
  mappend a b = Chunk (val a `mappend` val b) (eof a || eof b)
