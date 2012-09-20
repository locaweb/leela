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
       , pipeline
       , newMultiplex
       , multiplex
       , proc
       ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Function (on)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Map as M
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

type Events = S.Seq Event

type Pipeline = Proc Events Events

procFoldEvent :: (Events -> Event) -> Pipeline
procFoldEvent f = pureF (S.singleton . f)

procMapEvent :: (Double -> Double) -> Pipeline
procMapEvent f = pureF (fmap (update id f))

meanE :: Events -> Event
meanE xs = fixE $ F.foldl1 plus xs
  where size       = S.length xs
        
        fixE       = update id (/ (fromIntegral size))
        
        plus e0 e1 = let t  = min (time e0) (time e1)
                         v  = val e0 + val e1
                     in temporal t v

summarizeE :: (S.Seq Double -> Double) -> Events -> Event
summarizeE f xs = temporal (F.minimum (fmap time xs)) (f (fmap val xs))

sumE :: Events -> Event
sumE = summarizeE (F.foldl' (+) 0)

prodE :: Events -> Event
prodE = summarizeE (F.foldl' (*) 1)

firstE :: Events -> Event
firstE xs = let (x S.:< _) = S.viewl xs
            in x

lastE :: Events -> Event
lastE xs = let (_ S.:> x) = S.viewr xs
           in x

medianE :: Events -> Event
medianE xs
    | odd l     = firstE (S.drop m sxs)
    | otherwise = meanE (S.take 2 $ S.drop m sxs)
  where l   = S.length xs
        m   = l `div` 2
        sxs = S.unstableSortBy (compare `on` val) xs

timediff :: Events -> Time
timediff xs = let e1 = time $ firstE xs
                  e0 = time $ lastE xs
              in diff e1 e0

proc :: Function -> Pipeline
proc (Window n m)       = windowBy ((n ==) . S.length) (S.splitAt m)
proc (TimeWindow n)     = windowBy ((> n) . timediff) (\xs -> (xs, S.empty))
proc Sum                = procFoldEvent sumE
proc Prod               = procFoldEvent prodE
proc Abs                = procMapEvent abs
proc Ceil               = procMapEvent (fromInteger . ceiling)
proc Floor              = procMapEvent (fromInteger . floor)
proc Round              = procMapEvent (fromInteger . round)
proc Truncate           = procMapEvent (fromInteger . truncate)
proc Mean               = procFoldEvent meanE
proc Median             = procFoldEvent medianE
proc Maximum            = procFoldEvent (F.maximumBy (compare `on` val))
proc Minimum            = procFoldEvent (F.maximumBy (compare `on` val))
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

newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

multiplex :: (Monad m) => Key -> Event -> StateT (Pipeline, M.Map Key Pipeline) m Events
multiplex k e = do { p <- getenv k
                   ; case (eval $ feed p (S.singleton e))
                     of Left f        -> putenv k f >> return S.empty
                        Right (o, i)  -> (when (not $ S.null i)) (feedback k i) >> return o
                   }