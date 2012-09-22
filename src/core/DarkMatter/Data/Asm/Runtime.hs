{-# LANGUAGE TupleSections #-}
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

module DarkMatter.Data.Asm.Runtime
       ( Events
       , Pipeline
       , Runtime
       , eofChunk
       , pipeline
       , newMultiplex
       , multiplex1
       , multiplex
       , broadcastEOF
       , proc
       , run
       ) where

import           Prelude hiding (null)
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Function (on)
import           Data.Monoid (mempty)
import           Data.List
import qualified Data.Map as M
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

type Events = ChunkC ([Event])

type Pipeline = Proc Events Events

type Runtime m a = StateT (Pipeline, M.Map Key Pipeline) m a

run :: (Monad m) => Runtime m a -> (Pipeline, M.Map Key Pipeline) -> m (a, (Pipeline, M.Map Key Pipeline))
run = runStateT

procFoldEvent :: (Events -> ChunkC Event) -> Pipeline
procFoldEvent f = pureF (fmap (:[]) . f)

procMapEvent :: (Double -> Double) -> Pipeline
procMapEvent f = pureF (fmap (fmap (update id f)))

meanE :: Events -> ChunkC Event
meanE c = fmap (foldl1 plus) c
  where size = fromIntegral $ length (chk c)
        
        plus e0 e1 = let t  = min (time e0) (time e1)
                         v  = (val e0 / size) + (val e1 / size)
                     in temporal t v

summarizeE :: ([Double] -> Double) -> Events -> ChunkC Event
summarizeE f = fmap mk
  where mk xs = temporal (minimum (fmap time xs)) (f (fmap val xs))

sumE :: Events -> ChunkC Event
sumE = summarizeE (foldl' (+) 0)

prodE :: Events -> ChunkC Event
prodE = summarizeE (foldl' (*) 1)

medianE :: Events -> ChunkC Event
medianE c0
    | odd l     = fmap (head . drop m) c
    | otherwise = meanE (fmap (take 2 . drop m) c)
  where l = length (chk c0)
        m = l `div` 2
        c = fmap (sortBy (compare `on` val)) c0

proc :: Function -> Pipeline
proc (Window n)         = windowBy ((>= n) . length . chk)
proc (TimeWindow _)     = undefined -- windowBy ((> n) . timediff) (\c -> (c, mempty))
proc Sum                = procFoldEvent sumE
proc Prod               = procFoldEvent prodE
proc Abs                = procMapEvent abs
proc Ceil               = procMapEvent (fromInteger . ceiling)
proc Floor              = procMapEvent (fromInteger . floor)
proc Round              = procMapEvent (fromInteger . round)
proc Truncate           = procMapEvent (fromInteger . truncate)
proc Mean               = procFoldEvent meanE
proc Median             = procFoldEvent medianE
proc Maximum            = procFoldEvent (fmap (maximumBy (compare `on` val)))
proc Minimum            = procFoldEvent (fmap (minimumBy (compare `on` val)))
proc (Arithmetic Div t) = procMapEvent (/ t)
proc (Arithmetic Sub t) = procMapEvent (\n -> n - t)
proc (Arithmetic Mul t) = procMapEvent (* t)
proc (Arithmetic Add t) = procMapEvent (+ t)

pipeline :: [Function] -> Pipeline
pipeline = foldr1 pipe . map proc

getenv :: (Monad m) => Key -> Runtime m (Pipeline, Pipeline)
getenv k = do { (z, m) <- get
              ; case M.lookup k m
                of Nothing -> return (z, z)
                   Just y  -> return (z, y)
              }

putenv :: (Monad m) => Key -> Pipeline -> Runtime m ()
putenv k p = do { (z, m) <- get
                ; put (z, M.insert k p m)
                }

eofChunk :: Events
eofChunk = new mempty True

newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

multiplex1 :: (Monad m) => Key -> Event -> Runtime m Events
multiplex1 k = multiplex k . new_ . (:[])

multiplex :: (Monad m) => Key -> Events -> Runtime m Events
multiplex k i = do { (z, p) <- getenv k
                   ; case (eval $ feed p i)
                     of Left f        
                          -> putenv k f >> return mempty
                        Right o
                          -> putenv k z >> return o
                   }

broadcastEOF :: (Monad m, Functor m) => Runtime m [(Key, Events)]
broadcastEOF = do { (_, m) <- get
                  ; broadcast (M.keys m)
                  }
  where broadcast []     = return []
        broadcast (k:xs) = liftM2 (:) (fmap (k,) (multiplex k eofChunk)) (broadcast xs)
