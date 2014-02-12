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
       , now
       , diff
       , seconds
       , fromUTC
       , dateTime
       , fromISO8601
       , fromSeconds
       , fromDateTime
       ) where

import           Data.Time
import           Control.Applicative
import           Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B
import           Data.Time.Calendar.OrdinalDate

newtype Time = Time { unTime :: UTCTime }
             deriving (Show, Eq, Ord)

seconds :: Time -> Double
seconds = realToFrac . utcTimeToPOSIXSeconds . unTime

dateTime :: Time -> (Integer, Int, Double)
dateTime (Time u) = let (year, dayofyear) = toOrdinalDate $ utctDay u
                        time              = realToFrac $ utctDayTime u
                    in (year, dayofyear, time)

diff :: Time -> Time -> Double
diff (Time a) (Time b) = realToFrac $ a `diffUTCTime` b

asInt :: (Num i) => B.ByteString -> Maybe i
asInt s = case (B.readInt s) of
           Just (n, "") -> Just (fromIntegral n)
           _            -> Nothing


fromSeconds :: Double -> Time
fromSeconds = Time . posixSecondsToUTCTime . realToFrac

fromDateTime :: Integer -> Int -> Double -> Time
fromDateTime year dayofyear time = Time $ UTCTime (fromOrdinalDate year dayofyear) (realToFrac time)

fromUTC :: UTCTime -> Time
fromUTC = undefined

fromISO8601 :: B.ByteString -> Maybe Time
fromISO8601 date
  | t == "T" && z == "Z" = liftA Time (liftA2 UTCTime
                             (fromGregorian <$> (asInt year) <*> (asInt month) <*> (asInt day))
                             ((+) <$> ((3600 *) <$> asInt hour) <*> ((+) <$> ((60 *) <$> asInt minute) <*> asInt second)))
  | otherwise            = Nothing
    where
      (year, monthPart) = B.splitAt 4 date
      (month, dayPart)  = B.splitAt 2 monthPart
      (day, timePart)   = B.splitAt 2 dayPart
      (t, hourPart)     = B.splitAt 1 timePart
      (hour, minPart)   = B.splitAt 2 hourPart
      (minute, secPart) = B.splitAt 2 minPart
      (second, z)       = B.splitAt 2 secPart

now :: IO Time
now = fmap Time getCurrentTime
