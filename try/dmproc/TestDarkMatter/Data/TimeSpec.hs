-- -*- mode: haskell; -*-
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

module TestDarkMatter.Data.TimeSpec where

import Test.Hspec
import Test.QuickCheck
import DarkMatter.Data.Time

zero_spec :: Spec
zero_spec = do
  describe "zero" $ do
    it "both second and nseconds must be zero" $ do
      (zero (mktime 0 0) `shouldBe` True)

diff_spec :: Spec
diff_spec = do
  describe "diff" $ do
    it "considers absolute difference" $ do
      (diff (mktime 0 1) (mktime 0 0) `shouldBe` mktime 0 1)
      (diff (mktime 0 0) (mktime 0 1) `shouldBe` mktime 0 1)

mktime_spec :: Spec
mktime_spec = do
  describe "mktime" $ do
    it "should take care of unbounded nseconds argument" $ do
      (mktime 0 (10^9) `shouldBe` mktime 1 0)

toDouble_spec :: Spec
toDouble_spec = do
  describe "toDouble" $ do
    it "test conversion" $ property $
      (forAll samples (\(s, n) -> toDouble (mktime s n) == fromIntegral s + fromIntegral n/1e9))

  where samples = do { s <- arbitrary `suchThat` (>= 0)
                     ; n <- arbitrary
                     ; return (s, n `mod` 10^9)
                     }
