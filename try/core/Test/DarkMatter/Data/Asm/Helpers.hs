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

import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           Test.QuickCheck
import           DarkMatter.Data.Time
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Render

genStr :: Gen B8.ByteString
genStr = fmap B8.pack (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['.'])

genClose :: Gen Asm
genClose = return Close

genProc :: Gen Asm
genProc = liftM2 Proc arbitrary (listOf1 arbitrary)

genEvent :: Gen Asm
genEvent = do { k <- genStr
              ; c <- arbitrary
              ; v <- arbitrary
              ; return (Event k c v)
              }

genSyncFunc :: Gen SyncFunc
genSyncFunc = do { f <- arbitrary
                 ; v <- arbitrary
                 ; elements [ Sum
                            , Prod
                            , Id
                            , Truncate
                            , Floor
                            , Ceil
                            , Round
                            , Mean
                            , Median
                            , Minimum
                            , Maximum
                            , Abs
                            , ArithmeticL f v
                            , ArithmeticR f v
                            ]
                  }

genAsyncFunc :: Gen AsyncFunc
genAsyncFunc = do { n <- arbitrary `suchThat` (> 0)
                  ; m <- arbitrary `suchThat` (>= n)
                  ; f <- arbitrary `suchThat` ((> 0) . length)
                  ; g <- arbitrary
                  ; v <- arbitrary
                  ; elements [ Window n f
                             , SMA n
                             , Sample n m
                             , ComparisonL g v
                             , ComparisonR g v
                             ]
                  }

instance Arbitrary SyncFunc where

  arbitrary = genSyncFunc

instance Arbitrary AsyncFunc where

  arbitrary = genAsyncFunc

instance Arbitrary Asm where
  
  arbitrary = oneof [ genEvent
                    , genProc
                    , genClose
                    ]

instance Arbitrary Time where

  arbitrary = do { s <- arbitrary `suchThat` (>= 0)
                 ; n <- arbitrary `suchThat` (>= 0)
                 ; return (mktime s n)
                 }

instance Arbitrary Mode where

  arbitrary = elements [Match (B8.singleton '.', const True)]

instance Arbitrary ArithOp where

  arbitrary = elements [Mul, Add, Div, Sub]

instance Arbitrary ComparisonOp where

  arbitrary = elements [Eq, Lt, Le, Gt, Ge, Ne]

instance Show Asm where

  show = toString . render