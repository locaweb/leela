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

module Leela.Storage.Backend.ZMQ.Protocol
    ( Query (..)
    , Reply (..)
    , encode
    , decode
    , cacheKey
    , cacheVal
    , cacheUnkey
    , cacheUnval
    , mapToLazyBS
    , cacheKeyGlob
    ) where

import           Data.List (intersperse)
import           Data.Maybe
import           Data.Monoid ((<>), mconcat)
import           Control.Monad
import           Leela.Helpers
import qualified Data.Serialize as E
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Leela.Storage.Graph (Limit)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Builder

data Query = MsgGetName GUID
           | MsgGetGUID User Tree Kind Node
           | MsgPutName User Tree Kind Node
           | MsgPutLink [(GUID, Label, GUID)]
           | MsgPutLabel [(GUID, Label)]
           | MsgGetLink GUID Label (Maybe GUID) Limit
           | MsgHasLink GUID Label GUID
           | MsgGetLabel GUID (Mode Label) Limit
           | MsgUnlink [(GUID, Label, Maybe GUID)]
           | MsgPutAttr [(GUID, Attr, Value, [Option])]
           | MsgPutTAttr [(GUID, Attr, Time, Value, [Option])]
           | MsgDelAttr [(GUID, Attr)]
           | MsgGetAttr GUID Attr
           | MsgGetTAttr GUID Attr Time Limit
           | MsgListAttr GUID (Mode Attr) Limit
           | MsgListTAttr GUID (Mode Attr) Limit
           | MsgDelete GUID

data Reply = DoneMsg
           | NameMsg User Tree Kind Node GUID
           | LinkMsg [GUID]
           | KAttrMsg Value
           | TAttrMsg [(Time, Value)]
           | NAttrMsg [Attr]
           | LabelMsg [Label]
           | FailMsg Int

decodeInt :: B.ByteString -> Maybe Int
decodeInt s = case (B8.readInt s) of
                Just (n, "") -> Just n
                _            -> Nothing

truncateInt :: Double -> Int
truncateInt = truncate

cacheKeyGlob :: Maybe GUID -> Attr -> L.ByteString
cacheKeyGlob Nothing (Attr "*")       = "*"
cacheKeyGlob Nothing (Attr a)         = 0x2a `L.cons` 0 `L.cons` a
cacheKeyGlob (Just (GUID g)) (Attr a) = g `L.append` (0 `L.cons` a)

cacheKey :: GUID -> Attr -> L.ByteString
cacheKey (GUID g) (Attr a) = toLazyBS 128 (lazyByteString g <> char7 '\0' <> lazyByteString a)

cacheUnkey :: B.ByteString -> (GUID, Attr)
cacheUnkey raw = let (guid, attr) = B.break (== 0) raw
                 in (guidFromBS guid, attrFromBS $ B.drop 1 attr)

cacheVal :: Time -> Value -> B.ByteString
cacheVal time value = E.encode (time, value)

cacheUnval :: B.ByteString -> Maybe (Time, Value)
cacheUnval = either (const Nothing) Just . E.decode

decodeValues :: [B.ByteString] -> Either Int [(Time, Value)]
decodeValues = go []
    where
      go acc (t:v:rest) =
        case (liftM2 (,) (decodeInt t) (either (const Nothing) Just $ E.decode v)) of
          Just (t1, v1) -> go ((fromSeconds $ fromIntegral t1, v1) : acc) rest
          Nothing       -> Left 599
      go acc []         = Right acc
      go _ _            = Left 599

encodeMode :: (AsLazyByteString s) => Limit -> GUID -> Mode s -> [L.ByteString]
encodeMode lim g (All Nothing)  = [ "all"
                                  , asLazyByteString g
                                  , L.empty
                                  , toLazyBS 32 $ intDec lim
                                  ]
encodeMode lim g (All (Just l)) = [ "all"
                                  , asLazyByteString g
                                  , asLazyByteString l
                                  , toLazyBS 32 $ intDec lim
                                  ]
encodeMode lim g (Prefix a b)   = [ "pre"
                                  , asLazyByteString g
                                  , asLazyByteString a
                                  , asLazyByteString b
                                  , toLazyBS 32 $ intDec lim
                                  ]
encodeMode _ g (Precise l)      = [ "ext"
                                  , asLazyByteString g
                                  , asLazyByteString l
                                  ]

encodeOptions :: [Option] -> L.ByteString
encodeOptions = toLazyBS 512 . mconcat . intersperse (string7 ", ") . mapMaybe encodeOption
    where
      encodeOption (TTL v)           = Just $ string7 "ttl:" <> (intDec v)
      encodeOption (Indexing)        = Just $ string7 "index:true"
      encodeOption (MaxDataPoints _) = Nothing
      encodeOption (Alignment _)     = Nothing

encode :: Query -> [L.ByteString]
encode (MsgGetName g)                  = [ "get"
                                         , "name"
                                         , asLazyByteString g
                                         ]
encode (MsgGetGUID u t k n)            = [ "get"
                                         , "guid"
                                         , asLazyByteString u
                                         , asLazyByteString t
                                         , asLazyByteString k
                                         , asLazyByteString n
                                         ]
encode (MsgHasLink a l b)              = [ "get"
                                         , "link"
                                         , asLazyByteString a
                                         , asLazyByteString l
                                         , asLazyByteString b
                                         , "1"
                                         ]
encode (MsgGetLink g l Nothing lim)    = [ "get"
                                         , "link"
                                         , asLazyByteString g
                                         , asLazyByteString l
                                         , L.empty
                                         , toLazyBS 32 $ intDec lim
                                         ]
encode (MsgGetLink g l (Just p) lim)   = [ "get"
                                         , "link"
                                         , asLazyByteString g
                                         , asLazyByteString l
                                         , asLazyByteString p
                                         , toLazyBS 32 $ intDec lim
                                         ]
encode (MsgGetLabel g m lim)           =   "get"
                                         : "label"
                                         : encodeMode lim g m
encode (MsgPutName u t k n)            = [ "put"
                                         , "name"
                                         , asLazyByteString u
                                         , asLazyByteString t
                                         , asLazyByteString k
                                         , asLazyByteString n
                                         ]
encode (MsgPutLink links)              =   "put"
                                         : "link"
                                         : (sConcatMap
                                              (\(a, l, b) -> [ asLazyByteString a
                                                             , asLazyByteString l
                                                             , asLazyByteString b
                                                             ]) links)
encode (MsgPutLabel labels)            =   "put"
                                         : "label"
                                         : (sConcatMap
                                              (\(a, l) -> [ asLazyByteString a
                                                          , asLazyByteString l
                                                          ]) labels)
encode (MsgUnlink links)               =   "del"
                                         : "link"
                                         : (sConcatMap
                                              (\(a, l, mb) -> [ asLazyByteString a
                                                              , asLazyByteString l
                                                              , maybe L.empty asLazyByteString mb
                                                              ]) links)
encode (MsgPutAttr attrs)              =   "put"
                                         : "k-attr"
                                         : (sConcatMap
                                              (\(g, a, v, o) -> [ asLazyByteString g
                                                                , asLazyByteString a
                                                                , E.encodeLazy v
                                                                , encodeOptions o
                                                                ]) attrs)

encode (MsgPutTAttr attrs)           =   "put"
                                       : "t-attr"
                                       : (sConcatMap
                                           (\(g, a, t, v, o) -> [ asLazyByteString g
                                                                , asLazyByteString a
                                                                , toLazyBS 32 $ intDec (truncateInt $ seconds t)
                                                                , E.encodeLazy v
                                                                , encodeOptions o
                                                                ]) attrs)
encode (MsgDelAttr attrs)            =   "del"
                                       : "k-attr"
                                       : (sConcatMap
                                            (\(g, a) -> [ asLazyByteString g
                                                        , asLazyByteString a
                                                        ]) attrs)
encode (MsgGetAttr g a)              = [ "get"
                                       , "k-attr"
                                       , asLazyByteString g
                                       , asLazyByteString a
                                       ]
encode (MsgGetTAttr g a t l)         = [ "get"
                                       , "t-attr"
                                       , asLazyByteString g
                                       , asLazyByteString a
                                       , toLazyBS 32 $ intDec (truncateInt $ seconds t)
                                       , toLazyBS 32 $ intDec l
                                       ]
encode (MsgListAttr g mode limit)    =   "get"
                                       : "attr"
                                       : "k-attr"
                                       : encodeMode limit g mode
encode (MsgListTAttr g mode limit)   =   "get"
                                       : "attr"
                                       : "t-attr"
                                       : encodeMode limit g mode
encode (MsgDelete a)                 = [ "del"
                                       , "node"
                                       , asLazyByteString a
                                       ]

decode :: [B.ByteString] -> Reply
decode ["done"]                = DoneMsg
decode ["name", u, t, k, n, g] = NameMsg (userFromBS u) (treeFromBS t) (kindFromBS k) (nodeFromBS n) (guidFromBS g)
decode ("link":guids)          = LinkMsg (map guidFromBS guids)
decode ("label":labels)        = LabelMsg (map labelFromBS labels)
decode ("n-attr":names)        = NAttrMsg (map attrFromBS names)
decode ["k-attr", value]       = either (const $ FailMsg 599) KAttrMsg (E.decode value)
decode ("t-attr":values)       = either FailMsg TAttrMsg $ decodeValues values
decode ["fail", code]          = maybe (FailMsg 599) id (fmap FailMsg (decodeInt code))
decode _                       = FailMsg 599

