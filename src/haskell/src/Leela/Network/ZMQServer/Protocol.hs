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

module Leela.Network.ZMQServer.Protocol
       ( FH
       , Request (..)
       , Response (..)
       , liftE
       , msgpack
       , msgpack1
       , msgunpack
       ) where

import Data.Word
import Data.Aeson
import Leela.Data.LQL
import Data.Attoparsec
import Data.ByteString (ByteString)
import Control.Exception
import Leela.Data.Excepts
import Control.Applicative
import Leela.Data.LQL.Comp
import Leela.Data.Namespace
import Data.Attoparsec.Char8
import Leela.Network.Protocol

type FH = Word64

data Request = Begin [LQL]
             | Fetch FH Int
             | Close FH

data Response = Channel Word64
              | Data Reply

parseRequest :: Namespace -> Parser Request
parseRequest n = do
  "begin " *> liftA Begin (parseLQL n)
  <|> "fetch " *> liftA2 Fetch (decimal `endBy` char ' ') (decimal `endBy` char ';')
  <|> "close " *> liftA Close (decimal `endBy` char ';')
    where endBy = liftA2 const

liftE :: SomeException -> Reply
liftE e = do
  case (fromException e) of
    Nothing              -> InternalError
    Just TimeoutExcept   -> InternalError
    Just BadDeviceExcept -> InternalError
    Just UserExcept      -> BadRequestError
    Just SystemExcept    -> InternalError
    Just NotFoundExcept  -> NotFoundError

msgunpack :: [ByteString] -> Request
msgunpack msg =
  case (chkloads (parseRequest tld) msg) of
    Left _           -> throw UserExcept
    Right (Begin []) -> throw UserExcept
    Right req        -> req

msgpack1 :: (ToJSON a) => a -> [ByteString]
msgpack1 x = msgpack [x]

msgpack :: (ToJSON a) => [a] -> [ByteString]
msgpack []     = [encodeStrict InternalError]
msgpack [x]    = [encodeStrict x]
msgpack (x:xs) = encodeStrict x : msgpack xs

instance ToJSON Response where

  toJSON (Channel n) = object [("done", object [("channel", toJSON n)])]
  toJSON (Data r)    = toJSON r
