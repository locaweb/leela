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

module TestDarkMatter.Data.Asm.ParserSpec where

import Blaze.ByteString.Builder
import Test.Hspec
import Test.QuickCheck
import TestDarkMatter.Helpers ()
import DarkMatter.Data.Asm.Types
import DarkMatter.Data.Asm.Parser
import DarkMatter.Data.Asm.Render

parseOneShouldBeOk :: Asm -> Bool
parseOneShouldBeOk asm = let f = runOne asmParser . toByteString . render
                         in f asm /= Nothing

parseAllShouldBeOk :: [Asm] -> Bool
parseAllShouldBeOk asms = let f = runAll asmParser . toByteString . renderList
                in length (f asms) == length asms

event_spec :: Spec
event_spec = do
  describe "event" $ do

    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isEvent
        samples1 = arbitrary `suchThat` \xs -> all isEvent xs && length xs > 0

proc_spec :: Spec
proc_spec = do
  describe "proc" $ do
    it "runOne"
      (forAll samples0 $ parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 $ parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isProc
        samples1 = arbitrary `suchThat` \xs -> all isProc xs && length xs > 0

close_spec :: Spec
close_spec = do
  describe "close" $ do
    it "runOne"
      (forAll samples0 parseOneShouldBeOk)

    it "runAll"
      (forAll samples1 parseAllShouldBeOk)

  where samples0 = arbitrary `suchThat` isClose
        samples1 = arbitrary `suchThat` \xs -> all isClose xs && length xs > 0
