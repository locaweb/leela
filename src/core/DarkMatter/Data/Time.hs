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

-- | The sole datatype that the core deals with.
module DarkMatter.Data.Time
       ( Time()
       , seconds
       , nseconds
       , fromUnixtimestamp
       , fromTime
       ) where

import System.Posix.Clock

newtype Time = Time (Int, Int)
             deriving (Ord, Eq)

integral :: (Num a) => Double -> a
integral = fromIntegral . trunc
  where trunc :: Double -> Integer
        trunc = truncate

fractional :: Double -> Double
fractional n = integral n - n

seconds :: Time -> Int
seconds (Time p) = fst p

nseconds :: Time -> Int
nseconds (Time p) = snd p

fromTime :: Int -> Int -> Time
fromTime = curry Time

fromUnixtimestamp :: Int -> Time
fromUnixtimestamp t = Time (t, 0)