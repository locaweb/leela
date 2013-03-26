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

module TestDarkMatter.Data.Wall.TimelineSpec where

import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import qualified DarkMatter.Data.Metric as M
import           DarkMatter.Data.Wall.Timeline

emptyWall :: Wall Int
emptyWall = empty

derive_spec :: Spec
derive_spec =
  describe "derive" $ do
    it "should emit Nothing on the first event" $ do
       let (_, me) = publish emptyWall (M.Derive 0 (mktime 0 0) 1)
       isNothing me `shouldBe` True

    it "should emit the difference after the second event" $ do
      let (w0, _)           = publish emptyWall (M.Derive 0 (mktime 0 0) 0)
          (e1, Just (_, e)) = publish w0 (M.Derive 0 (mktime 60 0) 60)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` 1

    it "should ignore events arriving faster than clock" $ do
      let (w0, _)           = publish emptyWall (M.Derive 0 (mktime 0 0) 0)
          (w1, me)          = publish w0 (M.Derive 0 (mktime 1 0) 10)
          (w2, Just (_, e)) = publish w1 (M.Derive 0 (mktime 60 0) 60)
      isNothing me `shouldBe` True
      time e       `shouldBe` (mktime 60 0)
      val e        `shouldBe` 1

    it "should accept negative values" $ do
      let (w0, _)           = publish emptyWall (M.Derive 0 (mktime 0 0) 61)
          (w1, Just (_, e)) = publish w0 (M.Derive 0 (mktime 60 0) 1)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` (-1)

counter_spec :: Spec
counter_spec =
  describe "counter" $ do
    it "should emit Nothing on the first event" $ do
       let (_, me) = publish emptyWall (M.Counter 0 (mktime 0 0) 1)
       isNothing me `shouldBe` True

    it "should emit the difference after the second event" $ do
      let (w0, _)           = publish emptyWall (M.Counter 0 (mktime 0 0) 0)
          (e1, Just (_, e)) = publish w0 (M.Counter 0 (mktime 60 0) 60)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` 1

    it "should ignore events arriving faster than clock" $ do
      let (w0, _)           = publish emptyWall (M.Counter 0 (mktime 0 0) 0)
          (w1, me)          = publish w0 (M.Counter 0 (mktime 1 0) 10)
          (w2, Just (_, e)) = publish w1 (M.Counter 0 (mktime 60 0) 60)
      isNothing me `shouldBe` True
      time e       `shouldBe` (mktime 60 0)
      val e        `shouldBe` 1

    it "should wrap-counter negative values" $ do
      let (w0, _)           = publish emptyWall (M.Counter 0 (mktime 0 0) 61)
          (w1, Just (_, e)) = publish w0 (M.Counter 0 (mktime 60 0) 1)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` (2**32 - 61 + 1) / 60

    it "should wrap-counter negative values (II)" $ do
      let (w0, _)           = publish emptyWall (M.Counter 0 (mktime 0 0) 5e9)
          (w1, Just (_, e)) = publish w0 (M.Counter 0 (mktime 60 0) 1)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` (2**64 - 5e9 + 1) / 60

absolute_spec :: Spec
absolute_spec =
  describe "absolute" $ do
    it "should emit Nothing on the first event" $ do
       let (_, me) = publish emptyWall (M.Absolute 0 (mktime 0 0) 1)
       isNothing me `shouldBe` True

    it "should emit the difference after the second event" $ do
      let (w0, _)           = publish emptyWall (M.Absolute 0 (mktime 0 0) 60)
          (e1, Just (_, e)) = publish w0 (M.Absolute 0 (mktime 60 0) 0)
      time e `shouldBe` (mktime 60 0)
      val e  `shouldBe` 1

    it "should sum up events arriving faster than clock" $ do
      let (w0, _)           = publish emptyWall (M.Absolute 0 (mktime 0 0) 20)
          (w1, me0)         = publish w0 (M.Absolute 0 (mktime 15 0) 50)
          (w2, me1)         = publish w1 (M.Absolute 0 (mktime 30 0) 50)
          (w3, Just (_, e)) = publish w2 (M.Absolute 0 (mktime 65 0) 70)
      isNothing me0 `shouldBe` True
      isNothing me1 `shouldBe` True
      time e        `shouldBe` (mktime 60 0)
      val e         `shouldBe` 2

