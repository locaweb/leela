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

send_spec :: Spec
send_spec = do
    it "should be able to parse any \"send\" instructions"
      (forAll (arbitrary `suchThat` isSend) $ isRight . parse . render)

exec_spec :: Spec
exec_spec = do
    it "should be able to parse any \"exec\" instructions"
      (forAll (arbitrary `suchThat` isExec) $ isRight . parse . render)

free_spec :: Spec
free_spec = do
    it "should be able to parse any \"free\" instructions"
      (forAll (arbitrary `suchThat` isFree) $ isRight . parse . render)

specs :: Spec
specs = describe "Parser" $ do
    describe "send" send_spec
    describe "exec" exec_spec
    describe "free" free_spec
