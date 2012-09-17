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

module DarkMatter.Data.Asm.VM
       ( pipeline
       , proc
       , glob
       ) where

import qualified Data.Text as T
import           Data.Function (on)
import           Data.List (sortBy, maximumBy, minimumBy)
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

glob :: T.Text -> (T.Text -> Bool)
glob _ = const False

procFoldEvent :: ([Event] -> Event) -> Proc [Event] [Event]
procFoldEvent f = pureF ((:[]) . f)

procMapEvent :: (Double -> Double) -> Proc [Event] [Event]
procMapEvent f = pureF (map (update id f))

meanE :: [Event] -> Event
meanE xs = fixE $ foldl1 plus xs
  where size       = length xs
        
        fixE       = update id (/ (fromIntegral size))
        
        plus e0 e1 = let t  = min (time e0) (time e1)
                         k  = key e0
                         v  = val e0 + val e1
                     in temporal k t v

countE :: [Event] -> Event
countE xs = temporal (key (head xs)) (minimum (map time xs)) (sum (map val xs))

medianE :: [Event] -> Event
medianE xs
    | odd l     = head (drop m sxs)
    | otherwise = meanE (take 2 $ drop m sxs)
  where l   = length xs
        m   = l `div` 2
        sxs = sortBy (compare `on` val) xs

leftOp :: (Double -> Double -> Double) -> Double -> (Double -> Double)
leftOp f o = f o

rightOp :: (Double -> Double -> Double) -> Double -> (Double -> Double)
rightOp f o = flip f o

proc :: Function -> Proc [Event] [Event]
proc (Window n m)         = window n m
proc Count                = procFoldEvent countE
proc Abs                  = procMapEvent abs
proc Ceil                 = procMapEvent (fromInteger . ceiling)
proc Floor                = procMapEvent (fromInteger . floor)
proc Round                = procMapEvent (fromInteger . round)
proc Truncate             = procMapEvent (fromInteger . truncate)
proc Mean                 = procFoldEvent meanE
proc Median               = procFoldEvent medianE
proc Maximum              = procFoldEvent (maximumBy (compare `on` val))
proc Minimum              = procFoldEvent (minimumBy (compare `on` val))
proc (Arithmetic (Div t)) = procMapEvent (either (leftOp (/)) (rightOp (/)) t)
proc (Arithmetic (Sub t)) = procMapEvent (either (leftOp (-)) (rightOp (-)) t)
proc (Arithmetic (Mul t)) = procMapEvent (either (*) (*) t)
proc (Arithmetic (Add t)) = procMapEvent (either (+) (+) t)

pipeline :: [Function] -> Proc [Event] [Event]
pipeline = foldr (\f acc -> proc f `pipe` acc) (pureF id)