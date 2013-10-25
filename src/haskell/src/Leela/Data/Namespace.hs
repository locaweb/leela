{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

module Leela.Data.Namespace
    ( Key ()
    , Label ()
    , Namespace ()
    , Identifier (..)
    , Domain (..)
    , GUID ()
    -- * Top-level namespace
    , tld
    , Data.Hashable.hash
    -- * Querying
    , isDerivedOf
    -- * Hashing
    , guid
    , rehash
    ) where

import           Data.Word
import           Crypto.Hash
import           Data.Hashable
import           Data.Byteable
import qualified Data.ByteString as B
import           Control.Exception
import           Leela.Data.Excepts
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Base16

newtype GUID = GUID (Digest SHA224)
    deriving (Eq, Ord)

newtype Namespace = Namespace L.ByteString
    deriving (Eq, Ord, Show)

newtype Key = Key L.ByteString
    deriving (Eq, Ord, Show)

newtype Label = Label L.ByteString
    deriving (Eq, Ord, Show)

class Domain a where

  derive   :: Namespace -> a -> Namespace

  underive :: Namespace -> (a, Namespace)

class Identifier a b where

  pack :: b -> a

  unpack :: a -> b

sep :: Word8
sep = 0x2f

guid :: Namespace -> GUID
guid (Namespace l) = GUID (hashlazy l)

rehash :: GUID -> B.ByteString -> GUID
rehash (GUID g) b = GUID $ hashFinalize $ hashUpdates hashInit [toBytes g, b]

tld :: Namespace
tld = Namespace ""

isDerivedOf :: Namespace -> Namespace -> Bool
isDerivedOf (Namespace a) (Namespace b) = b `L.isPrefixOf` a

instance Domain B.ByteString where

  derive n = derive n . L.fromStrict

  underive n = let (s, n1) = underive n
               in (L.toStrict s, n1)

instance Domain L.ByteString where

  derive (Namespace n) s
    | sep `L.elem` s = throw UserExcept
    | otherwise      = Namespace (s `L.append` (sep `L.cons` n))

  underive (Namespace n) = let (s, n1) = L.break (== sep) n
                           in (s, Namespace $ L.drop 1 n1)

instance Domain Key where

  derive n (Key k) = derive n k

  underive n = let (s, n1) = underive n
               in (Key s, n1)

instance Domain Label where

  derive n (Label k) = derive n k

  underive n = let (s, n1) = underive n
               in (Label s, n1)

instance Identifier Namespace L.ByteString where

  pack = Namespace

  unpack (Namespace s) = s

instance Identifier Namespace B.ByteString where

  pack = Namespace . L.fromStrict

  unpack (Namespace s) = L.toStrict s

instance Identifier Key L.ByteString where

  pack = Key

  unpack (Key s) = s

instance Identifier Key B.ByteString where

  pack = Key . L.fromStrict

  unpack (Key s) = L.toStrict s

instance Identifier Label L.ByteString where

  pack = Label

  unpack (Label s) = s

instance Identifier Label B.ByteString where

  pack = Label . L.fromStrict

  unpack (Label s) = L.toStrict s

instance Identifier GUID B.ByteString where

  pack s = case (digestFromByteString (fst $ decode (B.drop 2 s))) of
             Nothing -> throw SystemExcept
             Just x  -> GUID x

  unpack (GUID s) = B.append "0x" (digestToHexByteString s)

instance Identifier GUID L.ByteString where

  pack s = (pack $ L.toStrict s)

  unpack g = L.fromStrict (unpack g)

instance Hashable GUID where

  hashWithSalt salt (GUID s) = hashWithSalt salt (toBytes s)
