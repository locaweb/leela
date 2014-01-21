-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
--    
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--    
--     http://www.apache.org/licenses/LICENSE-2.0
--    
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Leela.Data.Time
       ( Time
       , add
       , mul
       , now
       , diff
       , zero
       , mktime
       , sysnow
       , seconds
       , nseconds
       , toDouble
       ) where

import System.Clock
import Data.Time.Clock

newtype Time = Time { unTime :: (Int, Int) }
             deriving (Show, Eq, Ord)

seconds :: Time -> Int
seconds = fst . unTime

nseconds :: Time -> Int
nseconds = snd . unTime

toDouble :: Time -> Double
toDouble t =
  let s = fromIntegral (seconds t)
      n = fromIntegral (nseconds t)
  in s + n / nmax

mktime :: Int -> Int -> Time
mktime s n
    | s < 0 || n < 0 = error "mktime: negative numbers"
    | n < nmax       = Time (s, n)
    | otherwise      =
        let (s1, n1) = n `quotRem` nmax
        in Time (s+s1, n1)

zero :: Time -> Bool
zero t = seconds t == 0 && nseconds t == 0

nmax :: (Num a) => a
nmax = 1000000000

diff :: Time -> Time -> Time
diff t0 t1 =
  let s0     = abs $ seconds t1 - seconds t0
      (r, n) = (nseconds t1 - nseconds t0) `quotRem` nmax
      s      = abs $ s0 - (abs r)
  in mktime s (abs n)

add :: Time -> Time -> Time
add t0 t1 = mktime (seconds t0 + seconds t1) (nseconds t0 + nseconds t1)

mul :: Time -> Int -> Time
mul t x = mktime (x * seconds t) (nseconds t)

now :: IO Time
now = do
  t <- getTime Monotonic
  return (Time (sec t, nsec t))

sysnow :: IO UTCTime
sysnow = getCurrentTime
