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
    , zero
    , parse
    , parseFile
    , readSecret
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           System.IO
import           Data.Aeson
import           Data.Maybe
import qualified Data.Vector as V
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Types
import           Data.Text.Encoding
import           Control.Applicative
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as L
import           Leela.Data.Signature
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Builder

data Acl = RGraph
         | WGraph
         | RAttrs
         | WAttrs
         deriving (Ord, Eq, Show)

data UserInfo = UserInfo { secret :: Secret
                         , acl    :: M.Map L.ByteString (S.Set Acl)
                         }
              deriving (Eq, Show)

data Passwd = Passwd (M.Map User UserInfo)
            deriving (Eq, Show)

zero :: Passwd
zero = Passwd M.empty

can_ :: User -> Tree -> Acl -> UserInfo -> Bool
can_ (User u) (Tree t) perm info = let userOnly = S.member perm <$> M.lookup u (acl info)
                                       userTree = S.member perm <$> M.lookup (L.intercalate "::" [u, t]) (acl info)
                                   in fromMaybe (fromMaybe False userOnly) userTree

can :: Passwd -> User -> User -> Tree -> Acl -> Bool
can (Passwd db) caller user tree perm = fromMaybe False (can_ user tree perm <$> (M.lookup caller db))

readSecret :: Passwd -> User -> Maybe Secret
readSecret (Passwd db) user = secret <$> M.lookup user db

parse :: B.ByteString -> Maybe Passwd
parse = decodeStrict

parseFile :: FilePath -> IO (Maybe Passwd)
parseFile fh = withFile fh ReadMode
                 (fmap parse . flip B.hGetSome (1024 * 1024))

parsePerms :: String -> [Acl]
parsePerms = go [('r', RGraph), ('w', WGraph), ('r', RAttrs), ('w', WAttrs)]
    where
      go [] _         = []
      go _ []         = []
      go ((flag,perm):perms) (bit:bits)
        | bit == flag = perm : go perms bits
        | otherwise   = go perms bits

parseAcl :: (T.Text, T.Text) -> (L.ByteString, S.Set Acl)
parseAcl (what, perms) = (toLazyByteString $ encodeUtf8Builder what, S.fromList $ parsePerms $ T.unpack perms)

parseSecret :: B.ByteString -> Secret
parseSecret = initSecret . fst . B16.decode

instance FromJSON UserInfo where

  parseJSON (Array v) = UserInfo <$> ((parseSecret . encodeUtf8) <$> jsonSecret) <*> ((M.fromList . map parseAcl) <$> jsonAcls)
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
        parseEntry (k0, v0) = (User $ toLazyByteString $ encodeUtf8Builder k0,) <$> parseJSON v0
  parseJSON _          = mzero
