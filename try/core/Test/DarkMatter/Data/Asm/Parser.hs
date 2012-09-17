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

throw_spec :: Spec
throw_spec = do
    it "should be able to parse any \"throw\" instructions"
      (forAll (arbitrary `suchThat` isThrow) $ isRight . parse . render)

watch_spec :: Spec
watch_spec = do
    it "should be able to parse any \"watch\" instructions"
      (forAll (arbitrary `suchThat` isWatch) $ isRight . parse . render)

purge_spec :: Spec
purge_spec = do
    it "should be able to parse any \"purge\" instructions"
      (forAll (arbitrary `suchThat` isPurge) $ isRight . parse . render)

specs :: Spec
specs = describe "Parser" $ do
    describe "throw" throw_spec
    describe "watch" watch_spec
    describe "purge" watch_spec
