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
import           DarkMatter.Data.Time
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

renderTime :: Time -> T.Text
renderTime t = T.concat [ T.pack $ show $ seconds t
                        , "."
                        , T.pack $ show $ nseconds t
                        ]

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
renderFunction (TimeWindow t) = T.concat [ "time_window "
                                         , renderTime t
                                         ]
renderFunction (Window n m)   = T.concat [ "window "
                                         , T.pack $ show n
                                         , " "
                                         , T.pack $ show m
                                         ]

render :: Asm -> T.Text
render (Purge k)         = T.concat [ "purge "
                                    , T.pack $ show k
                                    ]
render (Throw k c v)     = T.concat [ "throw "
                                    , T.pack $ show k
                                    , " "
                                    , renderTime c
                                    , " "
                                    , T.pack $ show v
                                    ]
render (Watch k f)       = T.concat [ "watch "
                                    , T.pack $ show k
                                    , " "
                                    , renderPipeline f
                                    ]
