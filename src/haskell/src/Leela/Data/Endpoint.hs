{-# LANGUAGE OverloadedStrings #-}

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
       , isTCP
       , isUDP
       , toZmq
       , toZookeeper
       , loadEndpoint
       , dumpEndpoint
       , parseEndpoint
       , loadEndpointStr
       , dumpEndpointStr
       ) where

import           Data.Word
import           Data.Monoid
import           Data.Attoparsec as A
import qualified Data.ByteString as B
import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import           Data.Attoparsec.Char8 ((.*>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.ByteString.Lazy.Builder

data Endpoint = TCP { eAddr :: [(String, Maybe Word16)]
                    , ePath :: String
                    }
              | UDP { eAddr :: [(String, Maybe Word16)]
                    , ePath :: String
                    }
              deriving (Eq, Ord)

isTCP :: Endpoint -> Bool
isTCP (TCP{}) = True
isTCP _       = False

isUDP :: Endpoint -> Bool
isUDP (UDP{}) = True
isUDP _       = False

qstring :: (Word8 -> Bool) -> Parser (Maybe Word8, B.ByteString)
qstring p = cont []
    where
      cont acc = do
        eof <- atEnd
        if eof
          then return (Nothing, B.pack $ reverse acc)
          else go acc

      go acc = do
        c <- anyWord8
        case c of
          0x5c          -> anyWord8 >>= \c1 -> cont (c1:acc)
          _
            | p c       -> return (Just c, B.pack $ reverse acc)
            | otherwise -> cont (c:acc)

readWord :: B.ByteString -> Maybe Word16
readWord = fmap (fromIntegral . fst) . B8.readInt

splitColon :: B.ByteString -> (B.ByteString, Maybe B.ByteString)
splitColon s = let (l, r) = B.break (== 0x3a) s
               in if (B.null r)
                    then (s, Nothing)
                    else (l, Just $ B.drop 1 r)

splitAddr :: B.ByteString -> [(String, Maybe Word16)]
splitAddr = map (f . splitColon) . B.split 0x2c
    where
      f (h, mp) = (B8.unpack h, mp >>= readWord)

parseURL :: ([(String, Maybe Word16)] -> String -> a) -> Parser a
parseURL f = do
  (w, addr) <- qstring (`elem` [0x2f, 0x3b])
  case w of
    Just 0x2f -> do
      path <- fmap B8.unpack takeByteString
      if ((null path) || (last path) /= ';')
        then fail "bad path"
        else return (f (splitAddr addr) ('/' : init path))
    Just 0x3b -> return (f (splitAddr addr) "")
    _         -> fail "unknonw endpoint"

parseEndpoint :: Parser Endpoint
parseEndpoint =
  "tcp://" .*> parseURL TCP
  <|> "udp://" .*> parseURL UDP

escape :: Word8 -> B.ByteString -> B.ByteString
escape w s = let sep    = B.pack [0x5c, w]
                 chunks = B.split w s
             in B.intercalate sep chunks

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
    TCP addrs path -> toLazyByteString $ string7 "tcp://"
                      <> dumpAddrs addrs
                      <> string7 path
                      <> char7 ';'
    UDP addrs path -> toLazyByteString $ string7 "udp://"
                      <> dumpAddrs addrs
                      <> string7 path
                      <> char7 ';'
    where
      buildAddr (h, Nothing) = string7 h
      buildAddr (h, Just p)  = string7 h <> char7 ':' <> (string7 $ show p)

      dumpAddrs []     = error "empty addr"
      dumpAddrs (x:xs) = foldr (\a acc -> buildAddr a <> char7 ',' <> acc) (buildAddr x) xs

toZookeeper :: String -> Endpoint -> String
toZookeeper defEndpoint e
  | isTCP e   = maybe defEndpoint id $ showAddrs (eAddr e)
  | otherwise = defEndpoint
    where
      showAddrs []           = Nothing
      showAddrs (addr:addrs) = let f = \val acc -> acc ++ "," ++ showAddr val
                                   z = showAddr addr
                               in Just (foldr f z addrs ++  (ePath e))

      showAddr (h, Nothing) = h ++ ":2181"
      showAddr (h, Just p)  = h ++ ":" ++ show p

toZmq :: String -> Endpoint -> String
toZmq defEndpoint e
  | isTCP e   = maybe defEndpoint id $ showAddrs (eAddr e)
  | otherwise = defEndpoint
    where
      showAddrs []       = Nothing
      showAddrs (addr:_) = ("tcp://" ++) <$> showAddr addr

      showAddr (_, Nothing) = Nothing
      showAddr (h, Just p)  = Just $ h ++ ":" ++ show p

instance Read Endpoint where

  readsPrec _ s =
    case (loadEndpointStr s) of
      Nothing -> []
      Just e  -> [(e, "")]

instance Show Endpoint where

  show = dumpEndpointStr
