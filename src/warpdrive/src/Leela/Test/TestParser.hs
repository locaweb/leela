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
import qualified Data.ByteString as B
import           Test.Tasty.HUnit
import           Leela.Data.Types
import           Leela.Data.LQL.Read

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
  , testGroup "k-attr"
    []
  , testGroup "t-attr"
    []
  , testGroup "stat"
    [ testCase "stat command" $
        (count "using (leela) stat;") @?= 1
    ]
  , testGroup "make"
    [ testCase "make (a:a)" $
        (count "using (leela) make (a::a);") @?= 1
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
  , testGroup "guid"
    []
  , testGroup "path"
    [ testCase "path a" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000;") @?= 1
    , testCase "path a -[l]> b" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l]> 00000000-0000-0000-0000-000000000001;") @?= 1
    , testCase "path a -[l]> ()" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l]> ();") @?= 1
    , testCase "path a -[*l]> ()" $
        (count "using (leela) path 00000000-0000-0000-0000-000000000000 -[l*]> ();") @?= 1
    ]
  , testGroup "many"
    [ testCase "using ," $
      (count "using (leela) path 00000000-0000-0000-0000-000000000000, make (a::a);" @?= 2)
    , testCase "using \\n" $
      (count "using (leela) path 00000000-0000-0000-0000-000000000001\nmake (a::a);" @?= 2)
    ]
  , testGroup "attr [k|t]ls"
    [ testCase "attr kls" $
      (count "using (leela) attr kls 00000000-0000-0000-0000-000000000000 \"*\";" @?= 1)
    , testCase "attr tls" $
      (count "using (leela) attr tls 00000000-0000-0000-0000-000000000000 \"*\";" @?= 1)
    ]
  , testGroup "attr get (key-value)"
    [ testCase "attr get"
      (count "using (leela) attr get 00000000-0000-0000-0000-000000000000 \"attr\";" @?= 1)
    ]
  , testGroup "attr get (time-series)"
    [ testCase "attr get"
      (count "using (leela) attr get 00000000-0000-0000-0000-000000000000 \"attr\" [0:86400];" @?= 1)
    ]
  , testGroup "attr put (key-value)"
    [ testCase "attr put : string"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" \"string\";" @?= 1)
    , testCase "attr put : double : decimal"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (double 3.14);" @?= 1)
    , testCase "attr put : double : e-notation"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (double 3.14e6);" @?= 1)
    , testCase "attr put : int32"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (int32 42);" @?= 1)
    , testCase "attr put : uint32"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (uint32 42);" @?= 1)
    , testCase "attr put : int64"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (int64 42);" @?= 1)
    , testCase "attr put : uint64"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (uint64 42);" @?= 1)
    , testCase "attr put : boolean : true"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (bool true);" @?= 1)
    , testCase "attr put : boolean : false"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" (bool false);" @?= 1)
    ]
  , testGroup "attr put (time-series)"
    [ testCase "attr put : string"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] \"string\";" @?= 1)
    , testCase "attr put : double : decimal"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (double 3.14);" @?= 1)
    , testCase "attr put : double : e-notation"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (double 3.14e6);" @?= 1)
    , testCase "attr put : int32"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (int32 42);" @?= 1)
    , testCase "attr put : uint32"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (uint32 42);" @?= 1)
    , testCase "attr put : int64"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (int64 42);" @?= 1)
    , testCase "attr put : uint64"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (uint64 42);" @?= 1)
    , testCase "attr put : boolean : true"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (bool true);" @?= 1)
    , testCase "attr put : boolean : false"
      (count "using (leela) attr put 00000000-0000-0000-0000-000000000000 \"attr\" [0] (bool false);" @?= 1)
    ]
  , testGroup "attr last"
    []
  ]
