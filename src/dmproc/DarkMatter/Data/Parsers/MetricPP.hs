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

-- | The sole purpose of this module is to provide a [byte] string
-- represetation of the Asm type such as `parse . render == id`.
module DarkMatter.Data.Parsers.MetricPP where

import Data.Monoid ((<>))
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import DarkMatter.Data.Parsers.Helpers
import DarkMatter.Data.Metric

render :: (k -> Builder) -> Metric k -> Builder
render f m = renderType
             <> f (key m)
             <> fromChar ' '
             <> renderDouble (val m)
             <> fromChar ' '
             <> renderTime (time m)
             <> fromChar ';'
  where renderType
          | isGauge m    = fromByteString "gauge "
          | isDerive m   = fromByteString "derive "
          | isCounter m  = fromByteString "counter "
          | isAbsolute m = fromByteString "absolute "
