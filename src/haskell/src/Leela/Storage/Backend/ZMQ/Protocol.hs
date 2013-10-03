{-# LANGUAGE OverloadedStrings #-}

-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

module Leela.Storage.Backend.ZMQ.Protocol
    ( Query (..)
    , Reply (..)
    , encode
    , decode
    ) where

import qualified Data.ByteString as B
import           Leela.Data.Namespace
import           Data.ByteString.Char8 (readInt)
import           Leela.Storage.Backend (Mode (..))

data Query = GetName GUID
           | PutName Namespace Key GUID
           | PutLink GUID [GUID]
           | PutLabel GUID [Label]
           | GetLink GUID (Maybe GUID)
           | GetLabel GUID (Mode Label)
           | Unlink GUID GUID

data Reply = Done
           | Name Namespace Key
           | Link [GUID]
           | Label [Label]
           | Fail Int

decodeInt :: B.ByteString -> Maybe Int
decodeInt s = case (readInt s) of
                Just (n, "") -> Just n
                _            -> Nothing

encodeMode :: GUID -> Mode Label -> [B.ByteString]
encodeMode g (All Nothing)  = ["all", unpack g]
encodeMode g (All (Just l)) = ["all", unpack g, unpack l]
encodeMode g (Prefix a b)   = ["pre", unpack g, unpack a, unpack b]
encodeMode g (Suffix a b)   = ["suf", unpack g, unpack a, unpack b]
encodeMode g (Precise l)    = ["ext", unpack g, unpack l]

encode :: Query -> [B.ByteString]
encode (GetName g)          = ["get", "name", unpack g]
encode (GetLink g Nothing)  = ["get", "link", unpack g]
encode (GetLink g (Just p)) = ["get", "link", unpack g, unpack p]
encode (GetLabel g m)       = "get" : "label" : encodeMode g m
encode (PutName n k g)      = ["put", "name", unpack g, unpack n, unpack k]
encode (PutLink g xs)       = "put" : "link" : unpack g : map unpack xs
encode (PutLabel g xs)      = "put" : "label" : unpack g : map unpack xs
encode (Unlink a b)         = ["del", "link", unpack a, unpack b]

decode :: [B.ByteString] -> Reply
decode ["done"]         = Done
decode ["name", n, k]   = Name (pack n) (pack k)
decode ("link":guids)   = Link (map pack guids)
decode ("label":labels) = Label (map pack labels)
decode ["fail", code]   = maybe (Fail 599) id (fmap Fail (decodeInt code))
decode _                = Fail 599
