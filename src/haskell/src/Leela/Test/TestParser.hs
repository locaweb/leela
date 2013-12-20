-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

import Test.Tasty
import Leela.Data.LQL
import Test.Tasty.HUnit
import Leela.Data.LQL.Comp
import Leela.Data.Namespace

count :: String -> Int
count = either (const 0) length . loads (parseLQL tld)

suite = testGroup "Parser"
  [ testGroup "kill"
    [ testCase "kill (a) -[l]> (b)" $
        (count "using (leela) kill (a) -[l]> (b);") @?= 1
    , testCase "kill a -[l]> ()" $
        (count "using (leela) kill (a) -[l]> ();") @?= 1
    , testCase "kill (a) <[l]- (b)" $
        (count "using (leela) kill (a) <[l]- (b);") @?= 1
    , testCase "kill () <[l]- (b)" $
        (count "using (leela) kill () <[l]- (b);") @?= 1
    , testCase "kill (a) -[l]- (b)" $
        (count "using (leela) kill (a) -[l]- (b);") @?= 1
    ]
  , testGroup "stat"
    [ testCase "stat (namespace == system)" $
        (count "using (system) stat;") @?= 1
    , testCase "stat (namespace /= system)" $
        (count "using (leela) stat;") @?= 0
    ]
  , testGroup "make"
    [ testCase "make (a)" $
        (count "using (leela) make (a);") @?= 1
    , testCase "make (a) -[l]> (b)" $
        (count "using (leela) make (a) -[l]> (b);") @?= 1
    , testCase "make (a) <[l]- (b)" $
        (count "using (leela) make (a) <[l]- (b);") @?= 1
    , testCase "make (a) -[l]- (b)" $
        (count "using (leela) make (a) -[l]- (b);") @?= 1
    ]
  , testGroup "name"
    [ testCase "name 0x..." $
        (count "using (leela) name 0x48837a787f07673545d9c610bcbcd8d46a2691a71966d856c197e69e;") @?= 1
    ]
  , testGroup "path"
    [ testCase "path (a)" $
        (count "using (leela) path (a);") @?= 1
    , testCase "path (a) -[l]> (b)" $
        (count "using (leela) path (a) -[l]> (b);") @?= 1
    , testCase "path (a) -[l]> ()" $
        (count "using (leela) path (a) -[l]> ();") @?= 1
    , testCase "path (a) -[l*]> ()" $
        (count "using (leela) path (a) -[l*]> (b);") @?= 1
    , testCase "path (a) -[*l]> ()" $
        (count "using (leela) path (a) -[l*]> (b);") @?= 1
    ]
  , testGroup "many"
    [ testCase "using ," $
      (count "using (leela) path (a), make (a), make (b);" @?= 2) -- grouping make statements
    , testCase "using \\n" $
      (count "using (leela) path (a)\nmake (a)\nmake (b);" @?= 2) -- grouping make statements
    ]
  ]
