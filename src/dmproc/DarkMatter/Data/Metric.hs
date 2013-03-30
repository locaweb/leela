{-# LANGUAGE BangPatterns #-}
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
module DarkMatter.Data.Metric
       ( Metric(..)
       , isGauge
       , isDerive
       , isCounter
       , isAbsolute
       ) where

import Data.Hashable
import DarkMatter.Data.Time

data Metric k = Gauge    { key  :: !k
                         , val  :: !Double
                         , time :: !Time
                         }
              | Counter  { key  :: !k
                         , val  :: !Double
                         , time :: !Time
                         }
              | Derive   { key  :: !k
                         , val  :: !Double
                         , time :: !Time
                         }
              | Absolute { key  :: !k
                         , val  :: !Double
                         , time :: !Time
                         }
              deriving (Show, Eq)

isGauge :: Metric k -> Bool
isGauge (Gauge _ _ _) = True
isGauge _             = False

isCounter :: Metric k -> Bool
isCounter (Counter _ _ _) = True
isCounter _               = False

isDerive :: Metric k -> Bool
isDerive (Derive _ _ _) = True
isDerive _              = False

isAbsolute :: Metric k -> Bool
isAbsolute (Absolute _ _ _) = True
isAbsolute _               = False

instance (Hashable k) => Hashable (Metric k) where
  hashWithSalt k m = hashWithSalt k (key m)

