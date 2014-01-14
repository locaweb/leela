{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Storage.Backend.ZMQ.Protocol
    ( Query (..)
    , Reply (..)
    , encode
    , decode
    ) where

import qualified Data.ByteString as B
import           Leela.Data.Naming as N
import qualified Data.ByteString.Char8 as B8
import           Leela.Storage.Backend (Mode (..), pageSize)

data Query = GetName GUID
           | GetGUID User Tree Node
           | PutName User Tree Node
           | PutLink GUID Label GUID
           | PutLabel GUID Label
           | GetLink GUID Label (Maybe GUID)
           | HasLink GUID Label GUID
           | GetLabel GUID (Mode Label)
           | Unlink GUID Label (Maybe GUID)
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

encodeMode :: GUID -> Mode Label -> [B.ByteString]
encodeMode g (All Nothing)  = ["all", toByteString g, "", encodeShow pageSize]
encodeMode g (All (Just l)) = ["all", toByteString g, toByteString l, encodeShow pageSize]
encodeMode g (Prefix a b)   = ["pre", toByteString g, toByteString a, toByteString b, encodeShow pageSize]
encodeMode g (Suffix a b)   = ["suf", toByteString g, toByteString a, toByteString b, encodeShow pageSize]
encodeMode g (Precise l)    = ["ext", toByteString g, toByteString l]

encode :: Query -> [B.ByteString]
encode (GetName g)            = ["get", "name", toByteString g]
encode (GetGUID u t n)        = ["get", "guid", toByteString u, toByteString t, toByteString n]
encode (HasLink a l b)        = ["get", "link", toByteString a, toByteString l , toByteString b, "1"]
encode (GetLink g l Nothing)  = ["get", "link", toByteString g, toByteString l, "", encodeShow pageSize]
encode (GetLink g l (Just p)) = ["get", "link", toByteString g, toByteString l, toByteString p, encodeShow pageSize]
encode (GetLabel g m)         = "get" : "label" : encodeMode g m
encode (PutName u t n)        = ["put", "name", toByteString u, toByteString t, toByteString n]
encode (PutLink a l b)        = ["put", "link", toByteString a, toByteString l, toByteString b]
encode (PutLabel a l)         = ["put", "label", toByteString a, toByteString l]
encode (Unlink a l Nothing)   = ["del", "link", toByteString a, toByteString l]
encode (Unlink a l (Just b))  = ["del", "link", toByteString a, toByteString l, toByteString b]
encode (Delete a)             = ["del", "node", toByteString a]

decode :: [B.ByteString] -> Reply
decode ["done"]             = DoneMsg
decode ["name", u, t, n, g] = NameMsg (User u) (Tree t) (Node n) (GUID g)
decode ("link":guids)       = LinkMsg (map GUID guids)
decode ("label":labels)     = LabelMsg (map Label labels)
decode ["fail", code]       = maybe (FailMsg 599) id (fmap FailMsg (decodeInt code))
decode _                    = FailMsg 599
