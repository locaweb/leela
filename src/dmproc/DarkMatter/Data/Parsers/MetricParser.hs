-- -*- mode: haskell; -*-
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
module DarkMatter.Data.Parser.Metrics
       ( metricParser
       ) where

import           Control.Monad
import           Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import           Data.ByteString as B
import           DarkMatter.Data.Parsers.Helpers
import           DarkMatter.Data.Metrics

metricParser :: Parser Metric
metricParser = do { c <- P8.peekChar
                  ; case (c)
                    of 'g' -> parseGauge
                       'c' -> parseCounter
                       'd' -> parseDerive
                       'a' -> parseAbsolute
                       _   -> fail "parseMetric: g|c|d|a were expected"
                  }

parseGauge :: Parser Metric
parseGauge = string "gauge " >> parseMetric Gauge

parseCounter :: Parser Metric
parseCounter = string "counter " >> parseMetric Counter

parseDerive :: Parser Metric
parseDerive = string "derive " >> parseMetric Derive

parseAbsolute :: Parser Metric
parseAbsolute = string "absolute " >> parseMetric Absolute

parseMetric :: (B.ByteString -> Double -> Time -> Metric) -> Parser Metric
parseMetric metric = do { k <- parseStr
                        ; _ <- P8.space
                        ; v <- parseVal
                        ; _ <- P8.space
                        ; t <- parseTime
                        ; return (metric k v t)
                        }
