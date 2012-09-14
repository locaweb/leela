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
module DarkMatter.Data.Event
       ( Event(row, col, time, val)
       , durable
       , volatile
       , volatileM
       ) where

import System.Posix.Clock
import Data.Word
import qualified Data.Text as T

-- | The event, the sole datatype [from user perspective] that the
-- core deals with.
data Event = Durable { row :: T.Text
                     , col :: Word32
                     , val :: Double
                     }
             -- ^ Creates an event that is durable, written to the storage engine
           | Volatile { row  :: T.Text
                      , time :: TimeSpec
                      , val  :: Double
                      }
             -- ^ Creates an volatile event, which is not written to the storage engine
           deriving (Show)

durable :: T.Text -> Word32 -> Double -> Event
durable = Durable

volatile :: T.Text -> TimeSpec -> Double -> Event
volatile = Volatile

volatileM :: T.Text -> Double -> IO Event
volatileM k v = do
  { t <- getTime Monotonic
  ; return (volatile k t v)
  }