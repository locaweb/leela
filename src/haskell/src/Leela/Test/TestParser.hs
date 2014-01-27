-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
--    
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--    
--     http://www.apache.org/licenses/LICENSE-2.0
--    
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Leela.Test.TestParser
       ( suite
       ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Leela.Data.Naming
import           Leela.Data.LQL.Comp
import qualified Data.ByteString as B

count :: String -> Int
count = either (const 0) length . loads (parseLQL $ User B.empty)

suite :: TestTree
suite = testGroup "Parser"
  [ testGroup "kill"
    [ testCase "kill a -[l]> b" $
        (count "using (leela) kill 00000000-0000-0000-0000-000000000000 -[l]> 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "kill a -[l]> ()" $
        (count "using (leela) kill 00000000-0000-0000-0000-000000000000 -[l]> ();") @?= 1
    , testCase "kill a <[l]- b" $
        (count "using (leela) kill 00000000-0000-0000-0000-000000000000 <[l]- 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "kill () <[l]- b" $
        (count "using (leela) kill () <[l]- 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "kill a -[l]- b" $
        (count "using (leela) kill 00000000-0000-0000-0000-000000000000 -[l]- 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "kill a" $
        (count "using (leela) kill 00000000-0000-0000-0000-000000000000;") @?= 1
    ]
  , testGroup "stat"
    [ testCase "stat command" $
        (count "using (leela) stat;") @?= 1
    ]
  , testGroup "make"
    [ testCase "make (a)" $
        (count "using (leela) make (a);") @?= 1
    , testCase "make a -[l]> b" $
        (count "using (leela) make 00000000-0000-0000-0000-000000000000 -[l]> 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "make a <[l]- b" $
        (count "using (leela) make 00000000-0000-0000-0000-000000000000 <[l]- 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "make a -[l]- b" $
        (count "using (leela) make 00000000-0000-0000-0000-000000000000 -[l]- 00000000-0000-0000-0000-000000000001;") @?= 1
    ]
  , testGroup "name"
    [ testCase "name g" $
        (count "using (leela) name 00000000-0000-0000-0000-000000000000;") @?= 1
    ]
  , testGroup "path"
    [ testCase "path a" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000;") @?= 1
    , testCase "path a -[l]> b" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l]> 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "path a -[l]> ()" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l]> ();") @?= 1
    , testCase "path a -[l*]> ()" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l*]> ();") @?= 1
    , testCase "path a -[*l]> ()" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l*]> ();") @?= 1
    ]
  , testGroup "many"
    [ testCase "using ," $
      (count "using (leela) path 00000000-0000-0000-0000-000000000000, make (a), make (b);" @?= 3)
    , testCase "using \\n" $
      (count "using (leela) path 00000000-0000-0000-0000-000000000001\nmake (a)\nmake (b);" @?= 3)
    ]
  ]
