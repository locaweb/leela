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

import Data.Text (Text, pack, unpack)
import Test.QuickCheck
import DarkMatter.Data.Asm.Types
import DarkMatter.Data.Asm.Render

genKey :: Gen Text
genKey = fmap pack $ (listOf1 $ elements (['a'..'z'] ++ ['0'..'9']))

genFetch :: Gen Asm
genFetch = do { k     <- genKey
              ; col_a <- arbitrary
              ; col_b <- arbitrary
              ; funcs <- arbitrary
              ; return (Fetch k (col_a, col_b) funcs)
              }

genWatch :: Gen Asm
genWatch = do { k     <- genKey
              ; funcs <- arbitrary
              ; return (Watch k funcs)
              }

genPurge :: Gen Asm
genPurge = do { k     <- genKey
              ; col_a <- arbitrary
              ; col_b <- arbitrary
              ; return (Purge k (col_a, col_b))
              }

genStore :: Gen Asm
genStore = do { k <- genKey
              ; c <- arbitrary
              ; v <- arbitrary
              ; return (Store k c v)
              }

genThrow :: Gen Asm
genThrow = do { k <- genKey
              ; v <- arbitrary
              ; return (Throw k v)
              }

instance Arbitrary Asm where
  
  arbitrary = oneof [ genFetch
                    , genWatch
                    , genStore
                    , genThrow
                    , genPurge
                    ]

instance Arbitrary Function where
  
  arbitrary = do { n <- fmap abs arbitrary
                 ; m <- fmap abs arbitrary
                 ; elements [Window n m, Mean, Median, Min, Max]
                 }

instance Show Asm where
  
  show = unpack . render

instance Show Function where
  
  show = unpack . renderFunction