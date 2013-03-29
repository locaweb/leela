{-# LANGUAGE OverloadedStrings #-}
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
module DarkMatter.Data.Parsers.MetricParser
       ( metricParser
       ) where

import           Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import           Data.ByteString as B
import           Data.ByteString.Char8 as B8
import           DarkMatter.Data.Parsers.Helpers
import           DarkMatter.Data.Time
import           DarkMatter.Data.Metric

metricParser :: Parser (Metric B.ByteString)
metricParser = do { c <- P8.satisfy (`B8.elem` "gcda")
                  ; case (c)
                    of 'g' -> string "auge "    >> parseMetric Gauge
                       'c' -> string "ounter "  >> parseMetric Counter
                       'd' -> string "erive "   >> parseMetric Derive
                       'a' -> string "bsolute " >> parseMetric Absolute
                       _   -> fail "parseMetric: g|c|d|a were expected"
                  }

parseMetric :: (B.ByteString -> Double -> Time -> (Metric B.ByteString)) -> Parser (Metric B.ByteString)
parseMetric metric = do { k <- parseStr
                        ; _ <- P8.space
                        ; v <- parseVal
                        ; _ <- P8.space
                        ; t <- parseTime
                        ; return (metric k v t)
                        }
