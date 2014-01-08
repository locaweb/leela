{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

module Leela.Data.Namespace
    ( Key ()
    , Label ()
    , Namespace ()
    , Identifier (..)
    , Domain (..)
    , GUID ()
    -- * Top-level namespace
    , tld
    -- * Querying
    , isDerivedOf
    ) where

import           Data.Word
import qualified Data.ByteString as B
import           Control.Exception
import           Leela.Data.Excepts
import qualified Data.ByteString.Lazy as L

newtype GUID = GUID L.ByteString
    deriving (Eq, Ord, Show)

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

  pack = pack . L.fromStrict

  unpack = L.toStrict . unpack

instance Identifier GUID L.ByteString where

  pack = GUID

  unpack (GUID g) = g
