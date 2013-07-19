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
    -- * Casting
    , guid
    ) where

import           Data.Word
import           Data.Bits
import           Data.Aeson
import           Data.Maybe
import           Crypto.Hash
import           Data.Hashable
import           Data.Byteable
import qualified Data.ByteString as B
import           Data.Text.Encoding
import           Control.Applicative
import qualified Data.ByteString.Lazy as L

newtype GUID = GUID (Digest SHA224)
    deriving (Eq)

newtype Namespace = Namespace L.ByteString
    deriving (Eq, Show)

newtype Key = Key L.ByteString
    deriving (Eq, Ord, Show)

newtype Label = Label L.ByteString
    deriving (Eq, Ord, Show)

class Domain a where

    derive :: Namespace -> a -> Namespace

class Identifier a b where

    pack :: b -> a

    unpack :: a -> b

sep :: L.ByteString
sep = L.singleton 0x2f

guid :: Namespace -> GUID
guid (Namespace l) = GUID (hashlazy l)

tld :: Namespace
tld = Namespace ""

hex2word :: Word8 -> Maybe Word8
hex2word w
    | w >= 48 && w <= 57  = Just $ w - 48
    | w >= 65 && w <= 70  = Just $ w - 55
    | w >= 97 && w <= 102 = Just $ w - 87
    | otherwise           = Nothing

decodeWord :: Word8 -> Word8 -> Maybe Word8
decodeWord a b = do
  w0 <- hex2word a
  w1 <- hex2word b
  return (w0 `shiftL` 4 .|. w1)

decodeHex :: B.ByteString -> Maybe B.ByteString
decodeHex = fmap (B.pack . reverse) . go [] . B.unpack
    where go acc (w0:w1:ws) = decodeWord w0 w1 >>= \w -> go (w : acc) ws
          go acc []         = Just acc
          go _ _            = Nothing

instance Domain B.ByteString where

    derive n = derive n . L.fromStrict

instance Domain L.ByteString where

    derive (Namespace n) s = Namespace (n `L.append` sep `L.append` s)

instance Domain Key where

    derive n (Key k) = derive n k

instance Domain Label where

    derive n (Label k) = derive n k

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

    pack s = case (decodeHex (B.drop 2 s)) of
               Just  v -> GUID (fromJust $ digestFromByteString v)
               Nothing -> error "data corruption"
    unpack (GUID s) = B.append "0x" (digestToHexByteString s)

instance Identifier GUID L.ByteString where

    pack s = (pack $ L.toStrict s)
    unpack g = L.fromStrict (unpack g)

instance ToJSON Key where

    toJSON (Key w0) = toJSON w0

instance ToJSON Label where

    toJSON (Label w0) = toJSON w0

instance ToJSON GUID where

    toJSON g = toJSON (str)
        where str :: B.ByteString
              str = unpack g

instance ToJSON Namespace where

    toJSON (Namespace s) = toJSON s

instance FromJSON Namespace where

    parseJSON = withText "Namespace" $ pure . pack . encodeUtf8

instance FromJSON Key where

    parseJSON = withText "Key" $ pure . pack . encodeUtf8

instance FromJSON Label where

    parseJSON = withText "Label" $ pure . pack . encodeUtf8

instance FromJSON GUID where

    parseJSON = withText "GUID" $ pure . pack . encodeUtf8

instance Ord Namespace where

    compare (Namespace n0) (Namespace n1) = n0 `compare` n1

instance Hashable GUID where

    hashWithSalt salt (GUID s) = hashWithSalt salt (toBytes s)
