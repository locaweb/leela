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
       ( spec
       ) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Word
import qualified Data.Text as T
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Parser

store_spec :: Spec
store_spec = do
    it "should be able to parse \"store\" instructions with any col/val" $
      forAll input $ (Right True ==) . fmap isStore . compile
  where input = do { val <- arbitrary :: Gen Double
                   ; col <- arbitrary :: Gen Word32
                   ; return $ T.concat ["store \"spec.store\" ", T.pack $ show col, " ", T.pack $ show val]
                   }

throw_spec :: Spec
throw_spec = do
    it "should be able to parse \"throw\" instructions with any val" $
      forAll input $ (Right True ==) . fmap isThrow . compile
  where input = do { val <- arbitrary :: Gen Double
                   ; return $ T.concat ["throw \"spec.throw\" ", T.pack $ show val]
                   }

purge_spec :: Spec
purge_spec = do
    it "shoulbe be able to parse \"purge\" instructions with any range" $
      forAll input $ (Right True ==) . fmap isPurge . compile
  where input = do { col_a <- arbitrary :: Gen Word32
                   ; col_b <- arbitrary :: Gen Word32
                   ; return $ T.concat ["purge \"spec.purge\" ", T.pack $ show col_a, " ", T.pack $ show col_b]
                   }

spec :: Spec
spec = do
  describe "store" (store_spec)
  describe "throw" (throw_spec)
  describe "purge" (purge_spec)
