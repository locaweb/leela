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

module DarkMatter.Data.Asm.Runtime
       ( Events
       , Pipeline
       , eofChunk
       , pipeline
       , newMultiplex
       , multiplex
       , broadcastEOF
       , proc
       ) where

import           Prelude hiding (null)
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Function (on)
import           Data.Monoid (mempty)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Map as M
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

type Events = ChunkC (S.Seq Event)

type Pipeline = Proc Events Events

procFoldEvent :: (Events -> ChunkC Event) -> Pipeline
procFoldEvent f = pureF (fmap S.singleton . f)

procMapEvent :: (Double -> Double) -> Pipeline
procMapEvent f = pureF (fmap (fmap (update id f)))

meanE :: Events -> ChunkC Event
meanE c = fmap (F.foldl1 plus) c
  where size = fromIntegral $ S.length (chk c)
        
        plus !e0 e1 = let t  = min (time e0) (time e1)
                          v  = (val e0 / size) + (val e1 / size)
                      in temporal t v

summarizeE :: (S.Seq Double -> Double) -> Events -> ChunkC Event
summarizeE f = fmap mk
  where mk xs = temporal (F.minimum (fmap time xs)) (f (fmap val xs))

sumE :: Events -> ChunkC Event
sumE = summarizeE (F.foldl' (+) 0)

prodE :: Events -> ChunkC Event
prodE = summarizeE (F.foldl' (*) 1)

firstE :: S.Seq Event -> Event
firstE xs = let (x S.:< _) = S.viewl xs
            in x

lastE :: S.Seq Event -> Event
lastE xs = let (_ S.:> x) = S.viewr xs
           in x

medianE :: Events -> ChunkC Event
medianE c0
    | odd l     = fmap (firstE . S.drop m) c
    | otherwise = meanE (fmap (S.take 2 . S.drop m) c)
  where l = S.length (chk c0)
        m = l `div` 2
        c = fmap (S.unstableSortBy (compare `on` val)) c

timediff :: Events -> Time
timediff c = let xs = chk c
                 e1 = time $ firstE xs
                 e0 = time $ lastE xs
             in diff e1 e0

chkSplitAt :: Int -> Events -> (Events, Events)
chkSplitAt m c = let (l, r) = S.splitAt m (chk c)
                 in (new_ l, new r (eof c))

proc :: Function -> Pipeline
proc (Window n m)       = windowBy ((n ==) . S.length . chk) (chkSplitAt m)
proc (TimeWindow n)     = windowBy ((> n) . timediff) (\xs -> (xs, mempty))
proc Sum                = procFoldEvent sumE
proc Prod               = procFoldEvent prodE
proc Abs                = procMapEvent abs
proc Ceil               = procMapEvent (fromInteger . ceiling)
proc Floor              = procMapEvent (fromInteger . floor)
proc Round              = procMapEvent (fromInteger . round)
proc Truncate           = procMapEvent (fromInteger . truncate)
proc Mean               = procFoldEvent meanE
proc Median             = procFoldEvent medianE
proc Maximum            = procFoldEvent (fmap (F.maximumBy (compare `on` val)))
proc Minimum            = procFoldEvent (fmap (F.maximumBy (compare `on` val)))
proc (Arithmetic Div t) = procMapEvent (/ t)
proc (Arithmetic Sub t) = procMapEvent (\n -> n - t)
proc (Arithmetic Mul t) = procMapEvent (* t)
proc (Arithmetic Add t) = procMapEvent (+ t)

pipeline :: [Function] -> Pipeline
pipeline = foldr (\f acc -> proc f `pipe` acc) (pureF id)

getenv :: (Monad m) => Key -> StateT (Pipeline, M.Map Key Pipeline) m Pipeline
getenv k = do { (z, m) <- get
              ; case M.lookup k m
                of Nothing -> return z
                   Just y  -> return y
              }

putenv :: (Monad m) => Key -> Pipeline -> StateT (Pipeline, M.Map Key Pipeline) m ()
putenv k p = do { (z, m) <- get
                ; put (z, M.insert k p m)
                }

feedback :: (Monad m) => Key -> Events -> StateT (Pipeline, M.Map Key Pipeline) m ()
feedback k es = do { (z, m) <- get
                   ; put (z, M.alter (go z) k m)
                   }
  where go z Nothing  = Just (feed z es)
        go _ (Just f) = Just (feed f es)

eofChunk :: Events
eofChunk = new mempty True

newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

multiplex :: (Monad m) => Key -> Event -> StateT (Pipeline, M.Map Key Pipeline) m Events
multiplex k e = do { p <- getenv k
                   ; case (eval $ feed p (new_ $ S.singleton e))
                     of Left f        -> putenv k f >> return mempty
                        Right (o, i)  -> (when (not $ null i)) (feedback k i) >> return o
                   }

broadcastEOF :: (Monad m) => StateT (Pipeline, M.Map Key Pipeline) m [(Key, Events)]
broadcastEOF = do { (p, m) <- get
                  ; put (p, M.empty)
                  ; return (broadcast (M.toList m))
                  }
  where broadcast []         = []
        broadcast ((k,f):xs) = case (eval $ feed f eofChunk)
                               of Right (o, _) -> (k, o) : broadcast xs
                                  _            -> broadcast xs