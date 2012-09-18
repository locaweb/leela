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

import Test.Hspec
import Test.QuickCheck
import Test.DarkMatter.Data.Asm.Helpers ()
import DarkMatter.Data.Asm.Types
import DarkMatter.Data.Asm.Parser
import DarkMatter.Data.Asm.Render

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

write_spec :: Spec
write_spec = do
    it "should be able to parse any \"write\" instructions"
      (forAll (arbitrary `suchThat` isWrite) $ isRight . parse . render)

creat_spec :: Spec
creat_spec = do
    it "should be able to parse any \"creat\" instructions"
      (forAll (arbitrary `suchThat` isCreat) $ isRight . parse . render)

close_spec :: Spec
close_spec = do
    it "should be able to parse any \"close\" instructions"
      (forAll (arbitrary `suchThat` isClose) $ isRight . parse . render)

flush_spec :: Spec
flush_spec = do
    it "should be able to parse any \"flush\" instructions"
      (forAll (arbitrary `suchThat` isFlush) $ isRight . parse . render)

specs :: Spec
specs = describe "Parser" $ do
    describe "creat" creat_spec
    describe "write" write_spec
    describe "close" close_spec
    describe "flush" flush_spec
