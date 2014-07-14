{-# LANGUAGE OverloadedStrings #-}

-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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
       , Date (..)
       , TimeSpec (..)
       , add
       , now
       , diff
       , sleep
       , seconds
       , dateTime
       , snapshot
       , fromSeconds
       , fromDateTime
       , milliseconds
       ) where

import Data.Time
import System.Clock
import Control.DeepSeq
import Control.Concurrent
import Data.Time.Clock.POSIX

newtype Time = Time { unTime :: Double }
             deriving (Show, Eq, Ord)

newtype Date = Date (Int, Int, Int)
             deriving (Show, Eq, Ord)

add :: NominalDiffTime -> Time -> Time
add increment (Time t) = Time (t + realToFrac increment)

seconds :: Time -> Double
seconds = unTime

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000000)

milliseconds :: Time -> Double
milliseconds = (* 1000) . unTime

dateTime :: Time -> (Date, Double)
dateTime (Time s) = let u                         = posixSecondsToUTCTime (realToFrac s)
                        time                      = realToFrac $ utctDayTime u
                        (year, month, dayOfMonth) = toGregorian $ utctDay u
                    in (Date (fromIntegral $ year, month, dayOfMonth), time)

diff :: Time -> Time -> Time
diff (Time a) (Time b) = Time (a - b)

fromSeconds :: Double -> Time
fromSeconds = Time

fromDateTime :: Date -> Double -> Time
fromDateTime date time = fromSeconds $ realToFrac $ utcTimeToPOSIXSeconds $ UTCTime (fromDate date) (realToFrac time)

fromDate :: Date -> Day
fromDate (Date (y, m, d)) = fromGregorian (fromIntegral y) m d

toDate :: Day -> Date
toDate day = let (year, month, dayOfMonth) = toGregorian day
             in Date (fromIntegral year, month, dayOfMonth)

fromTimeSpec :: TimeSpec -> Time
fromTimeSpec t = let a = fromIntegral $ sec t
                     b = (fromIntegral $ nsec t) / 1e9
                 in Time (a + b)

snapshot :: IO Time
snapshot = fmap fromTimeSpec $ getTime Monotonic

now :: IO Time
now = fmap fromTimeSpec (getTime Realtime)

instance Enum Date where

  succ = toDate . succ . fromDate
  pred = toDate . pred . fromDate

  toEnum   = toDate . toEnum
  fromEnum = fromEnum . fromDate

instance NFData Time where

  rnf (Time v) = rnf v
