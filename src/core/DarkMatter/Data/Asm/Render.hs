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

-- | The sole purpose of this module is to provide a string
-- represetation [mostly for testing/debug purposes/ of the Asm type
-- such as `parse . render == id`.
module DarkMatter.Data.Asm.Render
       ( render
       , renderFunction
       ) where

import qualified Data.Text as T
import           DarkMatter.Data.Asm.Types

renderPipeline :: [Function] -> T.Text
renderPipeline [] = ""
renderPipeline xs = " | " `T.append` (T.intercalate " | " (map renderFunction xs))

renderArithF :: ArithF -> T.Text
renderArithF f = case (f)
                      of Mul v -> myRender "*" v
                         Div v -> myRender "/" v
                         Add v -> myRender "+" v
                         Sub v -> myRender "-" v
  where myRender op (Left n)  = T.concat [T.pack $ show n, " ", op]
        myRender op (Right n) = T.concat [op, " ", T.pack $ show n]

renderFunction :: Function -> T.Text
renderFunction Mean           = "mean"
renderFunction Median         = "median"
renderFunction Maximum        = "maximum"
renderFunction Minimum        = "minimum"
renderFunction Count          = "count"
renderFunction Floor          = "floor"
renderFunction Ceil           = "ceil"
renderFunction Round          = "round"
renderFunction Truncate       = "truncate"
renderFunction Abs            = "abs"
renderFunction (Arithmetic f) = T.concat [ "("
                                         , renderArithF f
                                         , ")"
                                         ]
renderFunction (Window n m)   = T.concat [ "window "
                                         , T.pack $ show n
                                         , " "
                                         , T.pack $ show m
                                         ]

render :: Asm -> T.Text
render (Store k c v)     = T.concat [ "store \""
                                    , k
                                    , "\" "
                                    , T.pack $ show c
                                    , " "
                                    , T.pack $ show v
                                    ]
render (Throw k v)       = T.concat [ "throw \""
                                    , k
                                    , "\" "
                                    , T.pack $ show v
                                    ]
render (Fetch k (a,b) f) = T.concat [ "fetch \""
                                    , k
                                    , "\" "
                                    , T.pack $ show a
                                    , " "
                                    , T.pack $ show b
                                    , renderPipeline f
                                    ]
render (Watch k f)       = T.concat [ "watch \""
                                    , k
                                    , "\""
                                    , renderPipeline f
                                    ]
render (Purge k (a,b))   = T.concat [ "purge \""
                                    , k
                                    , "\" "
                                    , T.pack $ show a
                                    , " "
                                    , T.pack $ show b
                                    ]
