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

module Test.DarkMatter.Data.Asm.Helpers where

import Data.ByteString.Char8 (unpack, pack)
import Test.QuickCheck
import DarkMatter.Data.Time
import DarkMatter.Data.Asm.Types
import DarkMatter.Data.Asm.Render

genFlush :: Gen Asm
genFlush = return Flush

genCreat :: Gen Asm
genCreat = fmap Creat arbitrary

genData :: Gen Asm
genData = do { k <- fmap pack (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['.'])
             ; c <- arbitrary
             ; v <- arbitrary
             ; return (Data k c v)
             }

instance Arbitrary Asm where
  
  arbitrary = oneof [ genCreat
                    , genFlush
                    , genData
                    ]

instance Arbitrary Function where
  
  arbitrary = do { n <- fmap abs arbitrary
                 ; m <- fmap abs arbitrary
                 ; t <- arbitrary
                 ; f <- arbitrary
                 ; elements [ Window n m
                            , TimeWindow t
                            , Count
                            , Truncate
                            , Floor
                            , Ceil
                            , Round
                            , Mean
                            , Median
                            , Minimum
                            , Maximum
                            , Abs
                            , Arithmetic f
                            ]
                 }

instance Arbitrary Time where

  arbitrary = do { s <- fmap abs arbitrary
                 ; n <- fmap abs arbitrary
                 ; return (mktime s n)
                 }

instance Arbitrary ArithF where

  arbitrary = do { n <- arbitrary
                 ; elements [Mul n, Add n, Div n, Sub n]
                 }

instance Show Asm where
  
  show = unpack . render

instance Show Function where
  
  show = unpack . renderFunction