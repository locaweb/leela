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
       , window
       ) where

data Proc i o = Done o
              | Cont (i -> Proc i o)

-- | Produces a value
done :: o -> Proc i o
done = Done

-- | Requests more input
await :: (i -> Proc i o) -> Proc i o
await = Cont

-- | Feed everything into the proc until no more input is
-- available. Notice the output may not be a 1:1 correspondence in
-- to the input.
run :: Proc i o -> [i] -> [o]
run f = go f f
  where go _ _ []     = []
        go z g (x:xs) = case (eval g x)
                        of Right (o, Nothing) -> o : go z z xs
                           Right (o, Just l)  -> o : go z z (l:xs)
                           Left h             -> go z h xs

-- | Evaluates a single input. Right is used when the process has
-- produced a value, with a possibly leftover. Left is returned when
-- the process is requesting more input.
eval :: Proc i o -> i -> Either (Proc i o) (o, Maybe i)
eval (Done o) i = Right (o, Just i)
eval (Cont f) i = case f i
                  of Done o -> Right (o, Nothing)
                     h      -> Left h

-- | Apply a pure function over the proc.
apply :: (a -> b) -> Proc i a -> Proc i b
apply f (Done a) = done (f a)
apply f (Cont g) = await (\i -> apply f (g i))

-- | Connects two procs. It fully evaluatates the first proc and its
-- result is fed up into the second proc. Notice that if the second
-- proc does not requests input, the first one is completely ignored.
pipe :: Proc a a  -- ^ The first proc
     -> Proc a a  -- ^ The second proc
     -> Proc a a
pipe _ (Done a)           = done a
pipe (Done a) (Cont f)    = f a
pipe (Cont f) g0@(Cont g) = await (\i -> case (f i)
                                         of Done a -> g a
                                            h      -> h `pipe` g0)

-- | Uses a pure function as a proc.
pure :: (i -> o) -> Proc i o
pure f = await (done . f)

-- | Groups n items together and use a function to produce a value.
window :: Int          -- ^ How many items to group
       -> ([i] -> o)   -- ^ The function to use to produce a value
       -> Proc i o
window n f = go []
  where go !acc
          | length acc == n = done (f acc)
          | otherwise       = await (\i -> go (i:acc))

instance Functor (Proc i) where

  fmap = apply

