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
module DarkMatter.Data.Asm.Render where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Double.Conversion.ByteString
import           Data.Monoid ((<>), mempty)
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

renderPipeline :: [Function] -> Builder
renderPipeline []     = mempty
renderPipeline [f]    = renderFunction f
renderPipeline (f:fs) = renderFunction f
                        <> fromString " | "
                        <> renderPipeline fs

renderOp :: ArithOp -> Builder
renderOp Mul = fromChar '*'
renderOp Add = fromChar '+'
renderOp Div = fromChar '/'
renderOp Sub = fromChar '-'

renderDouble :: Double -> Builder
renderDouble = fromByteString . toShortest

renderTime :: Time -> Builder
renderTime t = fromShow (seconds t)
               <> fromChar '.'
               <> fromShow (nseconds t)

renderEvent :: Builder -> Event -> Builder
renderEvent k e = fromString "event "
                  <> k
                  <> fromChar ' '
                  <> renderTime (time e)
                  <> fromChar ' '
                  <> renderDouble (val e)
                  <> fromChar ';'

renderMode :: Mode -> Builder
renderMode Map          = fromString "map"
renderMode (Window n m) = fromString "window "
                          <> fromShow n
                          <> fromChar ' '
                          <> fromShow m

renderFunction :: Function -> Builder
renderFunction Mean             = fromString "mean"
renderFunction Median           = fromString "median"
renderFunction Maximum          = fromString "maximum"
renderFunction Minimum          = fromString "minimum"
renderFunction Sum              = fromString "sum"
renderFunction Id               = fromString "id"
renderFunction Prod             = fromString "prod"
renderFunction Floor            = fromString "floor"
renderFunction Ceil             = fromString "ceil"
renderFunction Round            = fromString "round"
renderFunction Truncate         = fromString "truncate"
renderFunction Abs              = fromString "abs"
renderFunction (Arithmetic o v) = fromChar '('
                                  <> renderOp o
                                  <> fromChar ' '
                                  <> renderDouble v
                                  <> fromChar ')'

renderKey :: B.ByteString -> Builder
renderKey k = fromShow (B.length k)
              <> fromString "|"
              <> fromByteString k

render :: Asm -> Builder
render Close         = fromString "close;"
render (Event k t v) = fromString "event "
                       <> renderKey k
                       <> fromChar ' '
                       <> renderTime t
                       <> fromChar ' '
                       <> renderDouble v
                       <> fromChar ';'
render (Proc m f)    = fromString "proc "
                       <> renderMode m
                       <> fromString " | "
                       <> renderPipeline f
                       <> fromChar ';'

toString :: Builder -> String
toString = B8.unpack . toByteString