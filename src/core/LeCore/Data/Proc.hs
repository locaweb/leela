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
       -- ^ Combinators
       , done
       , await
       , apply
       -- ^ Evaluators
       , run
       , eval
       -- ^ Connectors
       , pipe
       -- ^ Utility procs
       , pure
       , swindow
       , hwindow
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
run :: Proc (Chunk i) o -> [i] -> [o]
run f = go f f . fromList
  where go _ _ []     = []
        go z g (x:xs) = case (eval g x)
                        of Right (o, Nothing) -> o : go z z xs
                           Right (o, Just l)  -> o : go z z (l:xs)
                           Left h             -> go z h xs

-- | Evaluates a single input. Right is used when the process has
-- produced a value, with a possibly leftover. Left is returned when
-- the process is requesting more input.
eval :: Proc i o -> i -> Either (Proc i o) (o, Maybe i)
eval (Put o) i = Right (o, Just i)
eval (Get f) i = case f i
                  of Put o -> Right (o, Nothing)
                     h      -> Left h

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
                                            h      -> h `pipe` g0)

-- | Uses a pure function as a proc.
pure :: (i -> o) -> Proc i o
pure f = await (done . f)

-- | Apply a function of a group of n items. This is not enforced, as
-- an EOF value may force it to use less than n elements.
swindow :: Int         -- ^ How many items to group
       -> ([i] -> o)   -- ^ The function to use to produce a value
       -> Proc (Chunk i) o
swindow n f = go []
  where go !acc
          | length acc == n = done (f acc)
          | otherwise       = await (\i -> let v = val i
                                           in if (eof i)
                                              then done (f $ v : acc)
                                              else go (v:acc))

-- | Same as swindow, but enforce always n elements.
hwindow :: Int         -- ^ How many items to group
        -> ([i] -> o)  -- ^ The function to use to produce a value
        -> Proc (Chunk i) o
hwindow n f = go []
  where go !acc
          | length acc == n = done (f acc)
          | otherwise       = await (\i -> go (val i:acc))

instance Functor (Proc i) where

  fmap = apply

instance Functor Chunk where

  fmap f c = c { val = f (val c) }

instance (Monoid i) => Monoid (Chunk i) where

  mempty      = Chunk mempty False
  mappend a b = Chunk (val a `mappend` val b) (eof a || eof b)
