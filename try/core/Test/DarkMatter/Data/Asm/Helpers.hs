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

import           Blaze.ByteString.Builder
import           Control.Monad
import           Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Test.QuickCheck
import           DarkMatter.Data.Time
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Render

genKey :: Gen B8.ByteString
genKey = fmap B8.pack (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['.'])

genClose :: Gen Asm
genClose = return Close

genProc :: Gen Asm
genProc = fmap Proc arbitrary

genEvent :: Gen Asm
genEvent = do { k <- genKey
              ; c <- arbitrary
              ; v <- arbitrary
              ; return (Event k c v)
              }

instance Arbitrary Asm where
  
  arbitrary = oneof [ genEvent
                    , genProc
                    , genClose
                    ]

instance Arbitrary Function where
  
  arbitrary = do { n <- fmap abs arbitrary
                 ; t <- arbitrary
                 ; f <- arbitrary
                 ; v <- arbitrary
                 ; elements [ Window n
                            , TimeWindow t
                            , Sum
                            , Prod
                            , Truncate
                            , Floor
                            , Ceil
                            , Round
                            , Mean
                            , Median
                            , Minimum
                            , Maximum
                            , Abs
                            , Arithmetic f v
                            ]
                 }

instance Arbitrary Time where

  arbitrary = do { s <- fmap abs arbitrary
                 ; n <- fmap abs arbitrary
                 ; return (mktime s n)
                 }

instance Arbitrary ArithOp where

  arbitrary = elements [Mul, Add, Div, Sub]

instance Show Asm where
  
  show = map (chr . fromIntegral) . B.unpack . toByteString . render

instance Show Function where
  
  show = map (chr . fromIntegral) . B.unpack . toByteString . renderFunction