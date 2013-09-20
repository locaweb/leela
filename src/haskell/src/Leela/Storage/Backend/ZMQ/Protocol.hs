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
    ( Request (..)
    , Response (..)
    ) where

import Data.Aeson
import Leela.Data.Namespace

data Request = GetName GUID
             | PutName Namespace Key GUID
             | GetLink GUID
             | PutLink GUID [GUID]
             | GetLabel GUID
             | PutLabel GUID [Label]
             | Unlink GUID GUID

data Response = ROk
              | RName Namespace Key
              | RLink [GUID]
              | RLabel [Label]
              | RFail Int

i :: Int -> Value
i = toJSON

instance ToJSON Request where

  toJSON (GetName g)       = object [("code", i 0), ("data", toJSON g)]
  toJSON (PutName n k g)   = object [("code", i 1), ("data", toJSON (n, k, g))]
  toJSON (GetLabel g)      = object [("code", i 2), ("data", toJSON g)]
  toJSON (PutLabel g lbls) = object [("code", i 3), ("data", toJSON (g, lbls))]
  toJSON (GetLink g)       = object [("code", i 4), ("data", toJSON g)]
  toJSON (PutLink g lnks)  = object [("code", i 5), ("data", toJSON (g, lnks))]
  toJSON (Unlink a b)      = object [("code", i 6), ("data", toJSON (a, b))]

instance FromJSON Response where

  parseJSON = withObject "Response" $ \o -> do
    msg <- o .: "code"
    case (msg :: Int) of
      0 -> return ROk
      1 -> o .: "data" >>= return . (uncurry RName)
      2 -> o .: "data" >>= return . RLink
      3 -> o .: "data" >>= return . RLabel
      4 -> fmap RFail (o .: "data")
      _ -> return (RFail 502)
                           
