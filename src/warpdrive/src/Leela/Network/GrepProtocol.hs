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

module Leela.Network.GrepProtocol
    ( EventMessage (..)
    , encodeAEvent
    , encodeGEvent
    , decodeEventMessage
    , encodeEventMessage
    ) where

import           Control.Monad
import qualified Data.Serialize as S
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import           Leela.Network.Protocol

data EventMessage = ControlMsg Time Grep
                  | AttrDataMsg Time [AttrEvent]
                  | GraphDataMsg Time [GraphEvent]

encodeEventMessage :: EventMessage -> L.ByteString
encodeEventMessage = S.encodeLazy

decodeEventMessage :: B.ByteString -> Maybe EventMessage
decodeEventMessage = either (const Nothing) Just . S.decode

encodeAEvent :: GUID -> AttrEvent -> [L.ByteString]
encodeAEvent guid (TAttrPutEvent g a t v opts) = asLazyByteString guid
                                                 : "attr"
                                                 : "put"
                                                 : asLazyByteString g
                                                 : asLazyByteString a
                                                 : encodeDouble (seconds t)
                                                 : encodeValue v
encodeAEvent guid (TAttrDelEvent g a mt)       = [ asLazyByteString guid
                                                 , "attr"
                                                 , "del"
                                                 , asLazyByteString g
                                                 , asLazyByteString a
                                                 , maybe "" (encodeDouble . seconds) mt
                                                 ]
encodeAEvent guid (KAttrPutEvent g a v opts)   = asLazyByteString guid
                                                 : "attr"
                                                 : "put"
                                                 : asLazyByteString g
                                                 : asLazyByteString a
                                                 : encodeValue v
encodeAEvent guid (KAttrDelEvent g a)          = [ asLazyByteString guid
                                                 , "attr"
                                                 , "del"
                                                 , asLazyByteString g
                                                 , asLazyByteString a
                                                 ]

encodeGEvent :: GUID -> GraphEvent -> [L.ByteString]
encodeGEvent guid _                            = [ asLazyByteString guid ]

instance S.Serialize EventMessage where

  put (ControlMsg t g)    = sequence_ [S.putWord8 0, S.put t, S.put g]
  put (AttrDataMsg t es)  = sequence_ [S.putWord8 1, S.put t, S.put es]
  put (GraphDataMsg t es) = sequence_ [S.putWord8 2, S.put t, S.put es]

  get = S.getWord8 >>= \code ->
    case code of
      0 -> ControlMsg <$> S.get <*> S.get
      1 -> AttrDataMsg <$> S.get <*> S.get
      2 -> GraphDataMsg <$> S.get <*> S.get
      _ -> mzero

