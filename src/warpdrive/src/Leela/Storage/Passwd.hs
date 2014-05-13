{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

module Leela.Storage.Passwd
    ( Passwd ()
    , can_
    , can
    , parse
    , parseFile
    , readSecret
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           System.IO
import           Data.Aeson
import qualified Data.Vector as V
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Types
import           Control.Exception
import           Data.Text.Encoding
import qualified Data.HashMap.Strict as H
import           Control.Applicative

data Acl = RGraph
         | WGraph
         | RAttrs
         | WAttrs
         deriving (Ord, Eq, Show)

data UserInfo = UserInfo { secret :: B.ByteString
                         , acl    :: M.Map B.ByteString (S.Set Acl)
                         }
              deriving (Eq, Show)

data Passwd = Passwd (M.Map User UserInfo)
            deriving (Eq, Show)

can_ :: User -> Tree -> Acl -> UserInfo -> Bool
can_ (User u) (Tree t) perm info = let userOnly = (S.member perm) <$> (M.lookup u $ acl info)
                                       userTree = (S.member perm) <$> (M.lookup (B.intercalate "::" [u, t]) $ acl info)
                                   in maybe (maybe False id userOnly) id userTree

can :: Passwd -> User -> User -> Tree -> Acl -> Bool
can (Passwd db) caller user tree perm = maybe False id (can_ user tree perm <$> (M.lookup caller db))

readSecret :: Passwd -> User -> Maybe B.ByteString
readSecret (Passwd db) user = secret <$> M.lookup user db

parse :: B.ByteString -> Maybe Passwd
parse = decodeStrict

parseFile :: FilePath -> IO (Maybe Passwd)
parseFile fh = bracket (openFile fh ReadMode)
                       hClose
                       (fmap parse . flip B.hGetSome (1024 * 1024))

parsePerms :: String -> [Acl]
parsePerms = go [('r', RGraph), ('w', WGraph), ('r', RAttrs), ('w', WAttrs)]
    where
      go [] _         = []
      go _ []         = []
      go ((flag,perm):perms) (bit:bits)
        | bit == flag = perm : go perms bits
        | otherwise   = go perms bits

parseAcl :: (T.Text, T.Text) -> (B.ByteString, S.Set Acl)
parseAcl (what, perms) = (encodeUtf8 what, S.fromList $ parsePerms $ T.unpack perms)

instance FromJSON UserInfo where

  parseJSON (Array v) = UserInfo <$> (encodeUtf8 <$> jsonSecret) <*> ((M.fromList . map parseAcl) <$> jsonAcls)
      where
        jsonSecret
          | V.length v == 2 = parseJSON $ v V.! 0
          | otherwise       = mzero
        jsonAcls
          | V.length v == 2 = parseJSON $ v V.! 1
          | otherwise       = mzero
  parseJSON _         = mzero

instance FromJSON Passwd where

  parseJSON (Object o) = (Passwd . M.fromList) <$> mapM parseEntry (H.toList o)
      where
        parseEntry (k0, v0) = (User $ encodeUtf8 k0,) <$> parseJSON v0
  parseJSON _         = mzero
