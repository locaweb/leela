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
       ( Time
       , seconds
       , nseconds
       , mktime
       , diff
       ) where

newtype Time = Time { unTime :: (Int, Int) }
             deriving (Eq, Ord)

seconds :: Time -> Int
seconds = fst . unTime

nseconds :: Time -> Int
nseconds = snd . unTime

mktime :: Int -> Int -> Time
mktime s n = Time (s, n)
{-# INLINE mktime #-}

nmax :: Int
nmax = 10 ^ (9 :: Int)

diff :: Time -> Time -> Time
diff t0 t1 = let s      = seconds t1 - seconds t0
                 (r, n) = (nseconds t1 - nseconds t0) `divMod` nmax
             in mktime (s - r) n
