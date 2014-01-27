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

import qualified Data.ByteString as B
import           Leela.Data.Naming as N
import qualified Data.ByteString.Char8 as B8
import           Leela.Storage.Backend (Mode (..), Limit)

data Query = GetName GUID
           | GetGUID User Tree Node
           | PutName User Tree Node
           | PutLink [(GUID, Label, GUID)]
           | PutLabel [(GUID, Label)]
           | GetLink GUID Label (Maybe GUID) Limit
           | HasLink GUID Label GUID
           | GetLabel GUID (Mode Label) Limit
           | Unlink [(GUID, Label, Maybe GUID)]
           | Delete GUID

data Reply = DoneMsg
           | NameMsg User Tree Node GUID
           | LinkMsg [GUID]
           | LabelMsg [Label]
           | FailMsg Int

decodeInt :: B.ByteString -> Maybe Int
decodeInt s = case (B8.readInt s) of
                Just (n, "") -> Just n
                _            -> Nothing

encodeShow :: (Show s) => s -> B.ByteString
encodeShow = B8.pack . show

encodeMode :: Limit -> GUID -> Mode Label -> [B.ByteString]
encodeMode lim g (All Nothing)  = ["all", toByteString g, "", encodeShow lim]
encodeMode lim g (All (Just l)) = ["all", toByteString g, toByteString l, encodeShow lim]
encodeMode lim g (Prefix a b)   = ["pre", toByteString g, toByteString a, toByteString b, encodeShow lim]
encodeMode lim g (Suffix a b)   = ["suf", toByteString g, toByteString a, toByteString b, encodeShow lim]
encodeMode _ g (Precise l)    = ["ext", toByteString g, toByteString l]

encode :: Query -> [B.ByteString]
encode (GetName g)                = ["get", "name", toByteString g]
encode (GetGUID u t n)            = ["get", "guid", toByteString u, toByteString t, toByteString n]
encode (HasLink a l b)            = ["get", "link", toByteString a, toByteString l , toByteString b, "1"]
encode (GetLink g l Nothing lim)  = ["get", "link", toByteString g, toByteString l, "", encodeShow lim]
encode (GetLink g l (Just p) lim) = ["get", "link", toByteString g, toByteString l, toByteString p, encodeShow lim]
encode (GetLabel g m lim)         = "get" : "label" : encodeMode lim g m
encode (PutName u t n)            = ["put", "name", toByteString u, toByteString t, toByteString n]
encode (PutLink links)            = "put" : "link" : concatMap (\(a, l, b) -> [toByteString a, toByteString l, toByteString b]) links
encode (PutLabel labels)          = "put" : "label" : concatMap (\(a, l) -> [toByteString a, toByteString l]) labels
encode (Unlink links)             = "del" : "link" : concatMap (\(a, l, mb) -> [toByteString a, toByteString l, maybe B.empty toByteString mb]) links
encode (Delete a)                 = ["del", "node", toByteString a]

decode :: [B.ByteString] -> Reply
decode ["done"]             = DoneMsg
decode ["name", u, t, n, g] = NameMsg (User u) (Tree t) (Node n) (GUID g)
decode ("link":guids)       = LinkMsg (map GUID guids)
decode ("label":labels)     = LabelMsg (map Label labels)
decode ["fail", code]       = maybe (FailMsg 599) id (fmap FailMsg (decodeInt code))
decode _                    = FailMsg 599
