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
    ) where

import           Control.Monad
import qualified Data.Serialize as S
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Leela.Storage.Graph (Limit)
import qualified Data.ByteString.Char8 as B8

data Query = MsgGetName GUID
           | MsgGetGUID User Tree Node
           | MsgPutName User Tree Node
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
           | NameMsg User Tree Node GUID
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

decodeValues :: [B.ByteString] -> Either Int [(Time, Value)]
decodeValues = go []
    where
      go acc (t:v:rest) =
        case (liftM2 (,) (decodeInt t) (either (const Nothing) Just $ S.decode v)) of
          Just (t1, v1) -> go ((fromSeconds $ fromIntegral t1, v1) : acc) rest
          Nothing       -> Left 599
      go acc []         = Right acc
      go _ _            = Left 599

encodeShow :: (Show s) => s -> B.ByteString
encodeShow = B8.pack . show

encodeMode :: (AsByteString s) => Limit -> GUID -> Mode s -> [B.ByteString]
encodeMode lim g (All Nothing)  = ["all", toByteString g, "", encodeShow lim]
encodeMode lim g (All (Just l)) = ["all", toByteString g, toByteString l, encodeShow lim]
encodeMode lim g (Prefix a b)   = ["pre", toByteString g, toByteString a, toByteString b, encodeShow lim]
encodeMode lim g (Suffix a b)   = ["suf", toByteString g, toByteString a, toByteString b, encodeShow lim]
encodeMode _ g (Precise l)      = ["ext", toByteString g, toByteString l]

encodeOptions :: [Option] -> B.ByteString
encodeOptions = B.intercalate ", " . map encodeOption
    where
      encodeOption (TTL v)    = "ttl:" `B.append` (encodeShow v)
      encodeOption (Indexing) = "index:true"

encode :: Query -> [B.ByteString]
encode (MsgGetName g)                = ["get", "name", toByteString g]
encode (MsgGetGUID u t n)            = ["get", "guid", toByteString u, toByteString t, toByteString n]
encode (MsgHasLink a l b)            = ["get", "link", toByteString a, toByteString l , toByteString b, "1"]
encode (MsgGetLink g l Nothing lim)  = ["get", "link", toByteString g, toByteString l, "", encodeShow lim]
encode (MsgGetLink g l (Just p) lim) = ["get", "link", toByteString g, toByteString l, toByteString p, encodeShow lim]
encode (MsgGetLabel g m lim)         = "get" : "label" : encodeMode lim g m
encode (MsgPutName u t n)            = ["put", "name", toByteString u, toByteString t, toByteString n]
encode (MsgPutLink links)            = "put" : "link" : concatMap (\(a, l, b) -> [toByteString a, toByteString l, toByteString b]) links
encode (MsgPutLabel labels)          = "put" : "label" : concatMap (\(a, l) -> [toByteString a, toByteString l]) labels
encode (MsgUnlink links)             = "del" : "link" : concatMap (\(a, l, mb) -> [toByteString a, toByteString l, maybe B.empty toByteString mb]) links
encode (MsgPutAttr attrs)            = "put" : "k-attr" : concatMap (\(g, a, v, o) -> [ toByteString g
                                                                                      , toByteString a
                                                                                      , S.encode v
                                                                                      , encodeOptions o]) attrs
encode (MsgPutTAttr attrs)           = "put" : "t-attr" : concatMap (\(g, a, t, v, o) -> [ toByteString g
                                                                                         , toByteString a
                                                                                         , encodeShow (truncateInt $ seconds t)
                                                                                         , S.encode v
                                                                                         , encodeOptions o]) attrs
encode (MsgDelAttr attrs)            = "del" : "k-attr" : concatMap (\(g, a) -> [toByteString g, toByteString a]) attrs
encode (MsgGetAttr g a)              = ["get", "k-attr", toByteString g, toByteString a]
encode (MsgGetTAttr g a t l)         = ["get", "t-attr", toByteString g, toByteString a, encodeShow (truncateInt $ seconds t), encodeShow l]
encode (MsgListAttr g mode limit)    = "get" : "attr" : "k-attr" : encodeMode limit g mode
encode (MsgListTAttr g mode limit)   = "get" : "attr" : "t-attr" : encodeMode limit g mode
encode (MsgDelete a)                 = ["del", "node", toByteString a]

decode :: [B.ByteString] -> Reply
decode ["done"]             = DoneMsg
decode ["name", u, t, n, g] = NameMsg (User u) (Tree t) (Node n) (GUID g)
decode ("link":guids)       = LinkMsg (map GUID guids)
decode ("label":labels)     = LabelMsg (map Label labels)
decode ("n-attr":names)     = NAttrMsg (map Attr names)
decode ["k-attr", value]    = either (const $ FailMsg 599) KAttrMsg (S.decode value)
decode ("t-attr":values)    = either FailMsg TAttrMsg $ decodeValues values
decode ["fail", code]       = maybe (FailMsg 599) id (fmap FailMsg (decodeInt code))
decode _                    = FailMsg 599
