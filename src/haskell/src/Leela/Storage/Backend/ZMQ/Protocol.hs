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
import           Leela.Data.Namespace
import qualified Data.ByteString.Char8 as B8
import           Leela.Storage.Backend (Mode (..), pageSize)

data Query = GetName GUID
           | PutName Namespace Key GUID
           | PutLink GUID [GUID]
           | PutLabel GUID [Label]
           | GetLink GUID (Maybe GUID)
           | HasLink GUID GUID
           | GetLabel GUID (Mode Label)
           | Unlink GUID (Maybe GUID)

data Reply = Done
           | Name Namespace Key
           | Link [GUID]
           | Label [Label]
           | Fail Int

decodeInt :: B.ByteString -> Maybe Int
decodeInt s = case (B8.readInt s) of
                Just (n, "") -> Just n
                _            -> Nothing

encodeShow :: (Show s) => s -> B.ByteString
encodeShow = B8.pack . show

encodeMode :: GUID -> Mode Label -> [B.ByteString]
encodeMode g (All Nothing)  = ["all", unpack g, "", encodeShow pageSize]
encodeMode g (All (Just l)) = ["all", unpack g, unpack l, encodeShow pageSize]
encodeMode g (Prefix a b)   = ["pre", unpack g, unpack a, unpack b, encodeShow pageSize]
encodeMode g (Suffix a b)   = ["suf", unpack g, unpack a, unpack b, encodeShow pageSize]
encodeMode g (Precise l)    = ["ext", unpack g, unpack l]

encode :: Query -> [B.ByteString]
encode (GetName g)          = ["get", "name", unpack g]
encode (HasLink a b)        = ["get", "link", unpack a, unpack b, "1"]
encode (GetLink g Nothing)  = ["get", "link", unpack g, "0x", encodeShow pageSize]
encode (GetLink g (Just p)) = ["get", "link", unpack g, unpack p, encodeShow pageSize]
encode (GetLabel g m)       = "get" : "label" : encodeMode g m
encode (PutName n k g)      = ["put", "name", unpack g, unpack n, unpack k]
encode (PutLink g xs)       = "put" : "link" : unpack g : map unpack xs
encode (PutLabel g xs)      = "put" : "label" : unpack g : map unpack xs
encode (Unlink a Nothing)   = ["del", "link", unpack a]
encode (Unlink a (Just b))  = ["del", "link", unpack a, unpack b]

decode :: [B.ByteString] -> Reply
decode ["done"]         = Done
decode ["name", n, k]   = Name (pack n) (pack k)
decode ("link":guids)   = Link (map pack guids)
decode ("label":labels) = Label (map pack labels)
decode ["fail", code]   = maybe (Fail 599) id (fmap Fail (decodeInt code))
decode _                = Fail 599
