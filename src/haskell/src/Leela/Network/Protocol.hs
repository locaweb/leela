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

module Leela.Network.Protocol
    ( Reply (..)
    , RValue (..)
    , Stream (..)
    , consume
    , encodeLazy
    , encodeStrict
    , fromLink
    , fromNode
    , fromGUID
    , fromPath
    , done
    , isDone
    , isChunk
    ) where

import           Data.Aeson
import qualified Data.ByteString as B
import           Leela.Data.Graph (Link)
import           Leela.Data.Namespace
import qualified Data.ByteString.Lazy as L

data RValue = LinkVal Link
            | NodeVal GUID
            | PathVal GUID [(GUID, Label)]
            | NameVal GUID (Namespace, Key)

data Reply = Done
           | Chunk RValue
           | InternalError
           | NotFoundError
           | BadRequestError
           | TempUnavailError
           | NoSuchResourceError

data Stream b = Stream { runStream :: Reply -> IO (b, Stream b) }

fromLink :: Link -> Reply
fromLink = Chunk . LinkVal

fromNode :: GUID -> Reply
fromNode = Chunk . NodeVal

fromPath :: GUID -> [(GUID, Label)] -> Reply
fromPath g = Chunk . PathVal g

fromGUID :: GUID -> (Namespace, Key) -> Reply
fromGUID g k = Chunk $ NameVal g k

isChunk :: Reply -> Bool
isChunk (Chunk _) = True
isChunk _         = False

isDone :: Reply -> Bool
isDone Done = True
isDone _    = False

done :: Reply
done = Done

encodeStrict :: (ToJSON a) => a -> B.ByteString
encodeStrict = L.toStrict . encodeLazy

encodeLazy :: (ToJSON a) => a -> L.ByteString
encodeLazy = encode

consume :: [RValue] -> Stream b -> IO b
consume [] s     = fmap fst (runStream s Done)
consume (x:xs) s = fmap snd (runStream s (Chunk x)) >>= consume xs

instance ToJSON Reply where

  toJSON Done                       = object [("done", object [])]
  toJSON (Chunk (LinkVal c))        = object [("chunk", object [("link", toJSON c)])]
  toJSON (Chunk (NodeVal c))        = object [("chunk", object [("node", toJSON c)])]
  toJSON (Chunk (PathVal g c))      = object [("chunk", object [("path", toJSON (g, c))])]
  toJSON (Chunk (NameVal g (n, k))) = object [("chunk", object [("name", toJSON (g, n, k))])]
  toJSON InternalError              = object [("fail", object [("code", toJSON (500 :: Int))])]
  toJSON BadRequestError            = object [("fail", object [("code", toJSON (400 :: Int))])]
  toJSON NotFoundError              = object [("fail", object [("code", toJSON (404 :: Int))])]
  toJSON NoSuchResourceError        = object [("fail", object [("code", toJSON (410 :: Int))])]
  toJSON TempUnavailError           = object [("fail", object [("code", toJSON (503 :: Int))])]
