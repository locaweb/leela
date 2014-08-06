{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

module Leela.Data.TimeSeries
       ( alignSeries
       , maxDataPoints
       ) where

import qualified Data.Vector as V
import           Leela.Data.Time
import           Leela.Data.Types
import           Statistics.Sample

castToDouble :: Value -> Maybe Double
castToDouble (Text _)   = Nothing
castToDouble (Bool _)   = Nothing
castToDouble (Int32 v)  = Just $ fromIntegral v
castToDouble (Int64 v)  = Just $ fromIntegral v
castToDouble (UInt32 v) = Just $ fromIntegral v
castToDouble (UInt64 v) = Just $ fromIntegral v
castToDouble (Double v) = Just v

onlyNumeric :: [(Time, Value)] -> Maybe [(Time, Double)]
onlyNumeric = go id
    where
      go acc []            = Just (acc [])
      go acc ((t, v) : xs) =
        case (castToDouble v) of
          Nothing -> Nothing
          Just v' -> go (acc . ((t,v'):)) xs

groupBy :: Int -> [(Time, a)] -> [(Time, V.Vector a)]
groupBy n xs0 = go xs0
    where
      go [] = []
      go xs = let (as, bs) = splitAt n xs
              in (fst $ head as, V.fromList (map snd as)) : go bs

maxDataPoints :: Int -> [(Time, Value)] -> [(Time, Value)]
maxDataPoints maxPoints series
  | maxPoints <= 0 = series
  | otherwise      =
      case (length series `divMod` maxPoints) of
        (0,_) -> series
        (1,0) -> series
        (q,r) -> let summarize = (\(t, v) -> (t, Double $ mean v))
                 in maybe series (map summarize . groupBy (q + min 1 r)) (onlyNumeric series)

alignSeries :: Int -> [(Time, Value)] -> [(Time, Value)]
alignSeries by = map (\(t, v) -> (alignBy by t, v))
