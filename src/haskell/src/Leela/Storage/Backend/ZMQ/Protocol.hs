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

import           Data.Aeson
import qualified Data.Vector as V
import           Data.Aeson.Types
import           Leela.Data.Namespace

data Request = Resolve GUID
             | GetNode GUID
             | PutNode Namespace Key GUID
             | PutLink [(GUID, GUID, Label)]

data Response = Done
              | Name Namespace Key
              | Node [(GUID, Label)]
              | SomeError Int

i :: Int -> Value
i = toJSON

asTuple :: (FromJSON a, FromJSON b) => Value -> Parser (a, b)
asTuple = withArray "asTuple" go
    where go pair
              | V.length pair == 2 = do
            a <- parseJSON (pair V.! 0)
            b <- parseJSON (pair V.! 1)
            return (a, b)
              | otherwise          = fail "asTuple"

parseName :: Value -> Parser Response
parseName = fmap (uncurry Name) . asTuple

parseNode :: Value -> Parser Response
parseNode = withArray "parseNode" $ fmap Node . Prelude.mapM asTuple . V.toList

instance ToJSON Request where

  toJSON (Resolve g)     = object [("code", i 0), ("data", toJSON g)]
  toJSON (GetNode g)     = object [("code", i 1), ("data", toJSON g)]
  toJSON (PutNode n k g) = object [("code", i 2), ("data", toJSON (n, k, g))]
  toJSON (PutLink lnks)  = object [("code", i 3), ("data", toJSON lnks)]

instance FromJSON Response where

  parseJSON = withObject "Response" $ \o -> do
    msg <- fmap v (o .: "code")
    case msg of
      0 -> return Done
      1 -> o .: "data" >>= parseName
      2 -> o .: "data" >>= parseNode
      3 -> fmap SomeError (o .: "data")
      _ -> return (SomeError 502)
      where v :: Int -> Int
            v = id
                           
