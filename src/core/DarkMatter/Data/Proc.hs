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

module DarkMatter.Data.Proc
       ( Proc (..)
       -- ^ Combinators
       , pipeM2
       , pipeM
       , pipe
       , proc
       -- ^ Evaluatation
       , run
       , run_
       , run1
       , runWhile
       ) where

import Control.Applicative

-- | A purely functional automata as usually found in literature.
-- 
-- It is provided instances for @Functor@ [fmap over the output] and
-- @Applicative@.
newtype Proc i o = Auto { unAuto :: i -> (o, Proc i o) }

-- | Sequences two procs.
pipe :: Proc a b
     -> Proc b c
     -> Proc a c
pipe (Auto f) (Auto g) = Auto h
  where h a = let (b, f1) = f a
                  (c, g1) = g b
              in c `seq` (c, pipe f1 g1)

-- | Sequences two procs. The first proc may decide to delay the
-- output [= @Nothing@], in which case the second one does not get
-- called.
pipeM :: Proc a (Maybe b)
      -> Proc b c
      -> Proc a (Maybe c)
pipeM (Auto f) g0@(Auto g) = Auto h
  where h a = case (f a)
              of (Nothing, f1) -> (Nothing, f1 `pipeM` g0)
                 (Just b, f1)  -> let (c, g1) = g b
                                  in c `seq` (Just c, f1 `pipeM` g1)

-- | Works exactly as @pipeM@ but in this version the second proc also
-- uses @Maybe@.
pipeM2 :: Proc a (Maybe b)
       -> Proc b (Maybe c)
       -> Proc a (Maybe c)
pipeM2 (Auto f) g0@(Auto g) = Auto h
  where h a = case (f a)
              of (Nothing, f1) -> (Nothing, f1 `pipeM2` g0)
                 (Just b, f1)  -> let (mc, g1) = g b
                                  in mc `mseq` (mc, f1 `pipeM2` g1)

-- | Lifts a pure function into a proc.
proc :: (i -> o) -> Proc i o
proc f = Auto (\i -> (f i, proc f))

-- | Iterates over a list and collectes all values along with the last
-- version of the proc.
run :: Proc i o -> [i] -> ([o], Proc i o)
run = go []
  where go acc p []     = (acc, p)
        go acc p (x:xs) = let (o, p1) = unAuto p x
                          in go (o:acc) p1 xs

-- | Same as @run@ but do not keep intermediary results. This is
-- sightly more efficient than @run@, as it discards all results but
-- the last. It is an error to use this with an empty list.
run_ :: Proc i o -> [i] -> (o, Proc i o)
run_ _  []       = error "run_: empty list"
run_ p0 (x0:xs0) = go (unAuto p0 x0) xs0
  where go (o, p) []     = (o, p)
        go (_, p) (x:xs) = go (unAuto p x) xs

-- | The same as @run_ p . (:[])@.
run1 :: Proc i o -> i -> (o, Proc i o)
run1 p = run_ p . (:[])

-- | Consumes the input until no more items are available or a
-- predicate fails (mostly for testing purposes). Example:
-- @
--   runWhile isJust (takeProc 5) [1..]
-- @
runWhile :: (o -> Bool) -> Proc i o -> [i] -> ([o], Proc i o)
runWhile t = go []
  where go acc p []     = (acc, p)
        go acc p (x:xs) = let (o, p1) = unAuto p x
                          in if (t o)
                             then go (o:acc) p1 xs
                             else (acc, p)

mseq :: Maybe a -> b -> b
mseq Nothing  b = b
mseq (Just x) b = x `seq` b

instance Functor (Proc i) where

  fmap f g = g `pipe` (proc f)

instance Applicative (Proc i) where
  
  pure a    = Auto $ const (a, pure a)

  pf <*> pv = Auto (\i -> let (f, pf1) = unAuto pf i
                              (v, pv1) = unAuto pv i
                          in (f v, pf1 <*> pv1))
            
