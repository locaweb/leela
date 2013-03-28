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

module TestDarkMatter.Data.Parsers.MetricParserSpec where

import Data.ByteString (ByteString)
import Blaze.ByteString.Builder
import Test.Hspec
import Test.QuickCheck
import TestDarkMatter.Data.Parsers.Helpers ()
import DarkMatter.Data.Metric
import DarkMatter.Data.Parsers.Helpers
import DarkMatter.Data.Parsers.MetricParser
import DarkMatter.Data.Parsers.MetricPP

parseOneShouldBeOk :: Metric ByteString -> Bool
parseOneShouldBeOk asm = let f = runOne metricParser . toByteString . (render renderStr)
                         in f asm /= Nothing

parseAllShouldBeOk :: [Metric ByteString] -> Bool
parseAllShouldBeOk asms = let f = runAll metricParser . toByteString . (renderList (render renderStr))
                in length (f asms) == length asms

gauge_spec :: Spec
gauge_spec = do
  describe "gauge" $ do

    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isGauge
        samples1 = arbitrary `suchThat` \xs -> all isGauge xs && length xs > 0

derive_spec :: Spec
derive_spec = do
  describe "gauge" $ do

    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isDerive
        samples1 = arbitrary `suchThat` \xs -> all isDerive xs && length xs > 0

counter_spec :: Spec
counter_spec = do
  describe "gauge" $ do

    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isCounter
        samples1 = arbitrary `suchThat` \xs -> all isCounter xs && length xs > 0

absolute_spec :: Spec
absolute_spec = do
  describe "gauge" $ do

    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isAbsolute
        samples1 = arbitrary `suchThat` \xs -> all isAbsolute xs && length xs > 0
