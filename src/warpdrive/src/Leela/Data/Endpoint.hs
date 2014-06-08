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

module Leela.Data.Endpoint
       ( Endpoint (..)
       , StrEndpoint
       , isTCP
       , isUDP
       , loadEndpoint
       , dumpEndpoint
       , parseEndpoint
       , loadEndpointStr
       , dumpEndpointStr
       ) where

import           Data.Word
import           Data.Monoid
import qualified Data.ByteString as B
import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.ByteString.Lazy.Builder
import           Data.Attoparsec.ByteString.Char8 (decimal)

data Endpoint = TCP String Word16 String
              | UDP String Word16 String
              deriving (Eq, Ord)

type StrEndpoint = String

isTCP :: Endpoint -> Bool
isTCP (TCP _ _ _) = True
isTCP _           = False

isUDP :: Endpoint -> Bool
isUDP (UDP _ _ _) = True
isUDP _           = False

parseURL :: (String -> Word16 -> String -> a) -> Parser a
parseURL f = do
  host <- fmap B8.unpack $ A.takeWhile (/= 0x3a)
  _    <- A.word8 0x3a
  port <- decimal
  path <- fmap B8.unpack takeByteString
  return $ f host port path

parseEndpoint :: Parser Endpoint
parseEndpoint =
  "tcp://" *> parseURL TCP
  <|> "udp://" *> parseURL UDP

loadEndpointStr :: String -> Maybe Endpoint
loadEndpointStr = loadEndpoint . B8.pack

dumpEndpointStr :: Endpoint -> String
dumpEndpointStr = L8.unpack . dumpEndpoint

loadEndpoint :: B.ByteString -> Maybe Endpoint
loadEndpoint s =
  case (parseOnly parseEndpoint s) of
    Left _  -> Nothing
    Right e -> Just e

dumpEndpoint :: Endpoint -> L.ByteString
dumpEndpoint endpoint =
  case endpoint of
    TCP host port path
      -> toLazyByteString $ string7 "tcp://"
         <> string7 host
         <> char7 ':'
         <> string7 (show port)
         <> string7 path
    UDP host port path
      -> toLazyByteString $ string7 "udp://"
         <> string7 host
         <> char7 ':'
         <> string7 (show port)
         <> string7 path

instance Read Endpoint where

  readsPrec _ s =
    case (loadEndpointStr s) of
      Nothing -> []
      Just e  -> [(e, "")]

instance Show Endpoint where

  show = dumpEndpointStr
