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
       ( pipeline
       , execute
       , proc
       ) where

import           Data.Function (on)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Event
-- import DarkMatter.Data.Time
import DarkMatter.Data.Asm.Types

data Runtime = Runtime { on_send :: Int -> Event -> IO ()
                       , on_exec :: Int -> Proc Events Events -> IO ()
                       , on_free :: Int -> IO ()
                       }

type Events = S.Seq Event

procFoldEvent :: (Events -> Event) -> Proc Events Events
procFoldEvent f = pureF (S.singleton . f)

procMapEvent :: (Double -> Double) -> Proc Events Events
procMapEvent f = pureF (fmap (update id f))

meanE :: Events -> Event
meanE xs = fixE $ F.foldl1 plus xs
  where size       = S.length xs
        
        fixE       = update id (/ (fromIntegral size))
        
        plus e0 e1 = let t  = min (time e0) (time e1)
                         v  = val e0 + val e1
                     in temporal t v

countE :: Events -> Event
countE xs = temporal (F.minimum (fmap time xs)) (F.sum (fmap val xs))

first :: Events -> Event
first xs = let (x S.:< _) = S.viewl xs
           in x

medianE :: Events -> Event
medianE xs
    | odd l     = first (S.drop m sxs)
    | otherwise = meanE (S.take 2 $ S.drop m sxs)
  where l   = S.length xs
        m   = l `div` 2
        sxs = S.unstableSortBy (compare `on` val) xs

leftOp :: (Double -> Double -> Double) -> Double -> (Double -> Double)
leftOp f o = f o

rightOp :: (Double -> Double -> Double) -> Double -> (Double -> Double)
rightOp f o = flip f o

proc :: Function -> Proc Events Events
proc (Window n m)         = window n m
-- proc (TimeWindow _)       = error "todo:fixme"
proc Count                = procFoldEvent countE
proc Abs                  = procMapEvent abs
proc Ceil                 = procMapEvent (fromInteger . ceiling)
proc Floor                = procMapEvent (fromInteger . floor)
proc Round                = procMapEvent (fromInteger . round)
proc Truncate             = procMapEvent (fromInteger . truncate)
proc Mean                 = procFoldEvent meanE
proc Median               = procFoldEvent medianE
proc Maximum              = procFoldEvent (F.maximumBy (compare `on` val))
proc Minimum              = procFoldEvent (F.maximumBy (compare `on` val))
proc (Arithmetic (Div t)) = procMapEvent (either (leftOp (/)) (rightOp (/)) t)
proc (Arithmetic (Sub t)) = procMapEvent (either (leftOp (-)) (rightOp (-)) t)
proc (Arithmetic (Mul t)) = procMapEvent (either (*) (*) t)
proc (Arithmetic (Add t)) = procMapEvent (either (+) (+) t)

pipeline :: [Function] -> Proc Events Events
pipeline = foldr (\f acc -> proc f `pipe` acc) (pureF id)

execute :: Runtime -> Asm -> IO ()
execute e (Send k t v) = on_send k (temporal t v)
execute e (Exec k f)   = on_exec k (pipeline f)
execute e (Purge k)    = on_free k