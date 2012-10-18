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

-- | The sole datatype that the core deals with.
module Test.DarkMatter.Data.Asm.Parser
       ( specs
       ) where

import           Blaze.ByteString.Builder
import           Debug.Trace
import           Data.Maybe
import qualified Data.ByteString as B
import           Test.Hspec
import           Test.QuickCheck
import           Test.DarkMatter.Data.Asm.Helpers ()
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Parser
import           DarkMatter.Data.Asm.Render

myRunOne :: Asm -> Bool
myRunOne asm = let f = runOne asmParser . toByteString . render
               in f asm /= Nothing

myRunAll :: [Asm] -> Bool
myRunAll asms = let f = runAll asmParser . toByteString . renderList
                in length (f asms) == length asms

event_spec :: Spec
event_spec = do
    it "\"event\" instruction"
      (forAll samples0 myRunOne)

    it "\"event\" instructions"
      (forAll samples1 myRunAll)

  where samples0 = arbitrary `suchThat` isEvent
        samples1 = arbitrary `suchThat` \xs -> all isEvent xs && length xs > 0

proc_spec :: Spec
proc_spec = do
    it "\"proc\" instruction"
      (forAll samples0 $ myRunOne)

    it "\"proc\" instructions"
      (forAll samples1 $ myRunAll)

  where samples0 = arbitrary `suchThat` isProc
        samples1 = arbitrary `suchThat` \xs -> all isProc xs && length xs > 0

close_spec :: Spec
close_spec = do
    it "\"close\" instruction"
      (forAll samples0 myRunOne)

    it "\"close\" instructions"
      (forAll samples1 myRunAll)

  where samples0 = arbitrary `suchThat` isClose
        samples1 = arbitrary `suchThat` \xs -> all isClose xs && length xs > 0

specs :: Spec
specs = describe "DarkMatter.Data.Asm.Parser" $ do
    describe "event" event_spec
    describe "proc" proc_spec
    describe "close" close_spec
