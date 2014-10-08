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
       ( onTimeseries
       ) where

import qualified Data.Vector as V
import           Leela.Data.Time
import           Leela.Data.Types
import           Leela.Data.Pipeline

castToDouble :: Value -> Maybe Double
castToDouble (Text _)   = Nothing
castToDouble (Bool _)   = Nothing
castToDouble (Int32 v)  = Just $ fromIntegral v
castToDouble (Int64 v)  = Just $ fromIntegral v
castToDouble (UInt32 v) = Just $ fromIntegral v
castToDouble (UInt64 v) = Just $ fromIntegral v
castToDouble (Double v) = Just v

fromDouble :: Double -> Value
fromDouble = Double

onTimeseries :: ([(Time, Value)] -> IO ())
              -> [Pipeline m (V.Vector (Time, Double))]
              -> [(Time, Value)]
              -> IO [Pipeline m (V.Vector (Time, Double))]
onTimeseries write p xs
  | null p    = write xs >> return p
  | otherwise = case (onlyNumeric xs) of
                  Nothing -> write xs >> return p
                  Just ys -> runPipelineIO
                               (write . V.toList . V.map (fmap fromDouble))
                               (not . V.null) p (V.fromList ys)

onlyNumeric :: [(Time, Value)] -> Maybe [(Time, Double)]
onlyNumeric = go id
    where
      go acc []            = Just (acc [])
      go acc ((t, v) : xs) =
        case (castToDouble v) of
          Nothing -> Nothing
          Just v' -> go (acc . ((t,v'):)) xs
