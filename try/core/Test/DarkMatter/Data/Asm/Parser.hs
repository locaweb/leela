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
import qualified Data.ByteString as B
import           Test.Hspec
import           Test.QuickCheck
import           Test.DarkMatter.Data.Asm.Helpers ()
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Parser
import           DarkMatter.Data.Asm.Render

check :: Maybe (a, B.ByteString) -> Bool
check (Just (_, i)) = B.null i
check _             = False

event_spec :: Spec
event_spec =
    it "should be able to parse any \"event\" instructions"
      (forAll (arbitrary `suchThat` isEvent) $ check . runOne . toByteString . render)

proc_spec :: Spec
proc_spec =
    it "should be able to parse any \"proc\" instructions"
      (forAll (arbitrary `suchThat` isProc) $ check . runOne . toByteString . render)

close_spec :: Spec
close_spec =
    it "should be able to parse any \"close\" instructions"
      (forAll (arbitrary `suchThat` isClose) $ check . runOne . toByteString . render)

specs :: Spec
specs = describe "DarkMatter.Data.Asm.Parser" $ do
    describe "event" event_spec
    describe "proc" proc_spec
    describe "close" close_spec
