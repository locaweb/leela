{-# LANGUAGE OverloadedStrings #-}

-- -*- mode: haskell; -*-
-- All Rights Reserved.
--
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
--
--        http://www.apache.org/licenses/LICENSE-2.0
--
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.

module Leela.Data.Endpoint
       ( Endpoint (..)
       , endpoint
       , strEndpoint
       , parseEndpoint
       , isTCP
       , isUDP
       ) where

import           Data.Word
import           Data.Attoparsec
import           Control.Applicative
import           Data.Attoparsec.Char8 ((.*>))
import qualified Data.ByteString as B
import           Data.ByteString.Char8 (readInt, unpack, pack)

data Endpoint = TCP { eHost :: B.ByteString
                    , ePort :: Maybe Word16
                    , eUser :: Maybe B.ByteString
                    , ePass :: Maybe B.ByteString
                    , ePath :: B.ByteString
                    }
              | UDP { eHost :: B.ByteString
                    , ePort :: Maybe Word16
                    , eUser :: Maybe B.ByteString
                    , ePass :: Maybe B.ByteString
                    , ePath :: B.ByteString
                    }
              deriving (Show)

isTCP :: Endpoint -> Bool
isTCP (TCP _ _ _ _ _) = True
isTCP _               = False

isUDP :: Endpoint -> Bool
isUDP (UDP _ _ _ _ _) = True
isUDP _               = False

qstring :: (Word8 -> Bool) -> Parser (Word8, B.ByteString)
qstring p = go []
    where go acc = do
            c <- anyWord8
            case c of
              0x5c          -> anyWord8 >>= \c1 -> go (c1:acc)
              _
                | p c       -> return (c, B.pack $ reverse acc)
                | otherwise -> go (c:acc)

parseSepByColon :: (Word8 -> Bool) -> Parser (Word8, B.ByteString, Maybe B.ByteString)
parseSepByColon p = do
  (wl, l) <- qstring (\w -> w == 0x3a || p w)
  if (wl == 0x3a)
  then do (wr, r) <- qstring p
          return (wr, l, Just r)
  else return (wl, l, Nothing)

readWord :: B.ByteString -> Maybe Word16
readWord = fmap (fromIntegral . fst) . readInt

parseURL :: Parser (B.ByteString, Maybe Word16, Maybe B.ByteString, Maybe B.ByteString, B.ByteString)
parseURL = do
  (w, userOrHost, passOrPort) <- parseSepByColon (`elem` [0x40, 0x2f])
  case w of
    0x40 -> do
      (_, host, port) <- parseSepByColon (== 0x2f)
      path            <- takeByteString
      return (host, port >>= readWord, Just userOrHost, passOrPort, path)
    0x2f -> do
      path <- takeByteString
      return (userOrHost, passOrPort >>= readWord, Nothing, Nothing, path)
    _    -> fail "unknonw endpoint"

parseEndpoint :: Parser Endpoint
parseEndpoint = do
  "tcp://" .*> (fmap (\(a,b,c,d,e) -> TCP a b c d e) parseURL)
  <|> "udp://" .*> fmap (\(a,b,c,d,e) -> UDP a b c d e) parseURL

endpoint :: B.ByteString -> Maybe Endpoint
endpoint s = case (parseOnly parseEndpoint s) of
               Left _  -> Nothing
               Right e -> Just e

strEndpoint :: String -> Maybe Endpoint
strEndpoint = endpoint . pack
