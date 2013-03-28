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

-- | The sole purpose of this module is to provide a [byte] string
-- represetation of the Asm type such as `parse . render == id`.
module DarkMatter.Data.Parsers.AsmPP where

import Data.Monoid ((<>), mempty)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import DarkMatter.Data.Parsers.Helpers
import DarkMatter.Data.Asm.Types

renderPipeline :: [Function] -> Builder
renderPipeline []     = mempty
renderPipeline [f]    = renderFunction f
renderPipeline (f:fs) = renderFunction f
                        <> fromString " | "
                        <> renderPipeline fs

renderAOp :: ArithOp -> Builder
renderAOp Mul = fromChar '*'
renderAOp Add = fromChar '+'
renderAOp Div = fromChar '/'
renderAOp Sub = fromChar '-'

renderLOp :: ComparisonOp -> Builder
renderLOp Eq = fromChar '='
renderLOp Le = fromString "<="
renderLOp Lt = fromChar '<'
renderLOp Ge = fromString ">="
renderLOp Gt = fromChar '>'
renderLOp Ne = fromString "/="

renderSyncFunc :: SyncFunc -> Builder
renderSyncFunc Mean              = fromString "mean"
renderSyncFunc Median            = fromString "median"
renderSyncFunc Maximum           = fromString "maximum"
renderSyncFunc Minimum           = fromString "minimum"
renderSyncFunc Sum               = fromString "sum"
renderSyncFunc Id                = fromString "id"
renderSyncFunc Prod              = fromString "prod"
renderSyncFunc Floor             = fromString "floor"
renderSyncFunc Ceil              = fromString "ceil"
renderSyncFunc Round             = fromString "round"
renderSyncFunc Truncate          = fromString "truncate"
renderSyncFunc Abs               = fromString "abs"
renderSyncFunc (ArithmeticL o v) = fromChar '('
                                  <> renderAOp o
                                  <> fromChar ' '
                                  <> renderDouble v
                                  <> fromChar ')'
renderSyncFunc (ArithmeticR o v) = fromChar '('
                                  <> renderDouble v
                                  <> fromChar ' '
                                  <> renderAOp o
                                  <> fromChar ')'

renderAsyncFunc :: AsyncFunc -> Builder
renderAsyncFunc (Window n p)       = fromString "window "
                                     <> fromShow n
                                     <> fromChar ' '
                                     <> fromChar '('
                                     <> renderPipeline (map Right p)
                                     <> fromChar ')'
renderAsyncFunc (Sample n m)       = fromString "sample "
                                     <> fromShow n
                                     <> fromChar '/'
                                     <> fromShow m
renderAsyncFunc (SMA n)            = fromString "sma "
                                     <> fromShow n
renderAsyncFunc (EWMA a)           = fromString "ewma "
                                     <> renderDouble a
renderAsyncFunc (ComparisonL o v)  = fromChar '['
                                   <> renderDouble v
                                   <> fromChar ' '
                                   <> renderLOp o
                                   <> fromChar ']'
renderAsyncFunc (ComparisonR o v)  = fromChar '['
                                   <> renderDouble v
                                   <> fromChar ' '
                                   <> renderLOp o
                                   <> fromChar ']'

renderFunction :: Function -> Builder
renderFunction (Left f)  = renderAsyncFunc f
renderFunction (Right f) = renderSyncFunc f

renderMode :: Mode -> Builder
renderMode Stream    = fromString "stream"
renderMode (Match k) = fromString "match " <> renderStr (fst k)

render :: Asm -> Builder
render Close         = fromString "close;"
render (Event k t v) = fromString "event "
                       <> renderStr k
                       <> fromChar ' '
                       <> renderTime t
                       <> fromChar ' '
                       <> renderDouble v
                       <> fromChar ';'
render (Proc m f)    = fromString "proc "
                       <> renderMode m
                       <> fromChar ' '
                       <> renderPipeline f
                       <> fromChar ';'
