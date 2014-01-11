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
    [ testCase "kill 00 -[l]> 01" $
        (count "using (leela) kill 00 -[l]> 01;") @?= 1
    , testCase "kill a -[l]> ()" $
        (count "using (leela) kill 00 -[l]> ();") @?= 1
    , testCase "kill (a) <[l]- (b)" $
        (count "using (leela) kill 00 <[l]- 01;") @?= 1
    , testCase "kill () <[l]- (b)" $
        (count "using (leela) kill () <[l]- 01;") @?= 1
    , testCase "kill 00 -[l]- 01" $
        (count "using (leela) kill 00 -[l]- 01;") @?= 1
    , testCase "kill 00" $
        (count "using (leela) kill 00;") @?= 1
    ]
  , testGroup "stat"
    [ testCase "stat command" $
        (count "using (leela) stat;") @?= 1
    ]
  , testGroup "make"
    [ testCase "make (a)" $
        (count "using (leela) make (a);") @?= 1
    , testCase "make 00 -[l]> 01" $
        (count "using (leela) make 00 -[l]> 01;") @?= 1
    , testCase "make 00 <[l]- 01" $
        (count "using (leela) make 00 <[l]- 01;") @?= 1
    , testCase "make 00 -[l]- 01" $
        (count "using (leela) make 00 -[l]- 01;") @?= 1
    ]
  , testGroup "name"
    [ testCase "name ..." $
        (count "using (leela) name 00;") @?= 1
    ]
  , testGroup "path"
    [ testCase "path 00" $
        (count "using (leela) path 00;") @?= 1
    , testCase "path 00 -[l]> 00" $
        (count "using (leela) path 00 -[l]> 00;") @?= 1
    , testCase "path 00 -[l]> ()" $
        (count "using (leela) path 00 -[l]> ();") @?= 1
    , testCase "path 00 -[l*]> ()" $
        (count "using (leela) path 00 -[l*]> ();") @?= 1
    , testCase "path 00 -[*l]> ()" $
        (count "using (leela) path 00 -[l*]> ();") @?= 1
    ]
  , testGroup "many"
    [ testCase "using ," $
      (count "using (leela) path 00, make (a), make (b);" @?= 3)
    , testCase "using \\n" $
      (count "using (leela) path 01\nmake (a)\nmake (b);" @?= 3)
    ]
  ]
