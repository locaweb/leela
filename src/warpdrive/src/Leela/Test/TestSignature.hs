{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Test.TestSignature
       ( suite
       ) where

import Test.Tasty
import Test.Tasty.HUnit
import Leela.Data.Signature

suite :: TestTree
suite = testGroup "Signature"
  [ testGroup "verify"
    [ testCase "with a valid signature" $ do
        nonce  <- genNonce
        secret <- genSecret
        verify secret nonce "foobar" (sign secret nonce "foobar") @? "verify . sign /= True"
    , testCase "with a valid signature and different nonce" $ do
        nonce  <- genNonce
        secret <- genSecret
        not (verify secret nonce "foobar" (sign secret (nextNonce nonce) "foobar")) @? "verify (nonce) . sign == True"
    , testCase "with a valid signature and different message" $ do
        nonce  <- genNonce
        secret <- genSecret
        not (verify secret nonce "foobar" (sign secret nonce "foobaz")) @? "verify (msg) . sign == True"
    , testCase "nextNonce generates a different nonce" $ do
        nonce <- genNonce
        nonce /= (nextNonce nonce) @? "nonce == nextNonce"
    ]
  ]
