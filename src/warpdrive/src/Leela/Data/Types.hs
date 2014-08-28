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

module Leela.Data.Types
       ( GUID (..)
       , Kind (..)
       , Mode (..)
       , Node (..)
       , Tree (..)
       , User (..)
       , Attr (..)
       , Label (..)
       , Value (..)
       , Option (..)
       , Journal (..)
       , Matcher (..)
       , TimeRange (..)
       , AsLazyByteString (..)
       , glob
       , nextPage
       , setOpt
       , isPutNode
       , isPutLink
       , isDelLink
       , isPutTAttr
       , isPutLabel
       , isPutKAttr
       , isDelKAttr
       , attrFromBS
       , guidFromBS
       , kindFromBS
       , nodeFromBS
       , treeFromBS
       , userFromBS
       , labelFromBS
       , getMaxDataPoints
       ) where

import           Data.Int
import           Data.Word
import           Data.Hashable
import           Data.Serialize
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Control.Exception
import           Leela.Data.Excepts
import qualified Data.ByteString.Lazy as L

newtype GUID = GUID L.ByteString
             deriving (Eq, Ord, Show)

newtype Label = Label L.ByteString
              deriving (Eq, Ord, Show)

newtype Node = Node L.ByteString
             deriving (Eq, Ord, Show)

newtype User = User L.ByteString
             deriving (Eq, Ord, Show)

newtype Tree = Tree L.ByteString
             deriving (Eq, Ord, Show)

newtype Kind = Kind L.ByteString
             deriving (Eq, Ord, Show)

newtype Attr = Attr L.ByteString
             deriving (Eq, Ord, Show)

data Value = Bool Bool
           | Text L.ByteString
           | Int32 Int32
           | Int64 Int64
           | Double Double
           | UInt32 Word32
           | UInt64 Word64
           deriving (Show, Eq)

data TimeRange = Range Time Time
               deriving (Eq)

data Matcher = ByLabel GUID Label
             | ByNode GUID
             | ByEdge GUID Label GUID
             deriving (Eq)

data Journal = PutLink GUID Label GUID
             | PutLabel GUID Label
             | PutNode User Tree Kind Node
             | DelLink GUID Label (Maybe GUID)
             | DelNode GUID
             | DelKAttr GUID Attr
             | PutKAttr GUID Attr Value [Option]
             | DelTAttr GUID Attr TimeRange
             | PutTAttr GUID Attr Time Value [Option]
             deriving (Eq)

data Option = TTL Int
            | Indexing
            | MaxDataPoints Int
            | Data L.ByteString L.ByteString
            deriving (Eq)

data Mode a = All (Maybe a)
            | Prefix a a
            | Precise a

class AsLazyByteString a where

  asLazyByteString :: a -> L.ByteString

guidFromBS :: B.ByteString -> GUID
guidFromBS = GUID . L.fromStrict

kindFromBS :: B.ByteString -> Kind
kindFromBS = Kind . L.fromStrict

treeFromBS :: B.ByteString -> Tree
treeFromBS = Tree . L.fromStrict

labelFromBS :: B.ByteString -> Label
labelFromBS = Label . L.fromStrict

nodeFromBS :: B.ByteString -> Node
nodeFromBS = Node . L.fromStrict

userFromBS :: B.ByteString -> User
userFromBS = User . L.fromStrict

attrFromBS :: B.ByteString -> Attr
attrFromBS = Attr . L.fromStrict

isDelKAttr :: Journal -> Bool
isDelKAttr (DelKAttr {}) = True
isDelKAttr _             = False

isPutKAttr :: Journal -> Bool
isPutKAttr (PutKAttr {}) = True
isPutKAttr _             = False

isDelLink :: Journal -> Bool
isDelLink (DelLink {}) = True
isDelLink _            = False

isPutLink :: Journal -> Bool
isPutLink (PutLink {}) = True
isPutLink _            = False

isPutLabel :: Journal -> Bool
isPutLabel (PutLabel _ _) = True
isPutLabel _              = False

isPutNode :: Journal -> Bool
isPutNode (PutNode {}) = True
isPutNode _            = False

isPutTAttr :: Journal -> Bool
isPutTAttr (PutTAttr {}) = True
isPutTAttr _             = False

getMaxDataPoints :: [Option] -> Maybe Int
getMaxDataPoints []                  = Nothing
getMaxDataPoints (MaxDataPoints n:_) = Just n
getMaxDataPoints (_:xs)              = getMaxDataPoints xs

setOpt :: Option -> [Option] -> [Option]
setOpt o1 []       = [o1]
setOpt o1 (o : xs)
  | o `same` o1    = o1 : xs
  | otherwise      = o : setOpt o1 xs

    where
      same (TTL _) (TTL _)                     = True
      same (MaxDataPoints _) (MaxDataPoints _) = True
      same Indexing Indexing                   = True
      same _ _                                 = False

glob :: L.ByteString -> Mode L.ByteString
glob s
  | "*" == s           = All Nothing
  | L.isSuffixOf "*" s = uncurry Prefix (range $ L.init s)
  | otherwise          = Precise s
    where
      range str = (str, L.init str `L.snoc` (L.last str + 1))

nextPage :: Mode a -> a -> Mode a
nextPage (All _) l      = All (Just l)
nextPage (Prefix _ b) l = Prefix l b
nextPage _ _            = error "precise has no pagination"

instance Serialize Value where

  put (Bool v)   = do
    putWord8 0
    putWord8 (fromIntegral $ fromEnum v)
  put (Text v)   = do
    putWord8 1
    putWord16be (fromIntegral $ L.length v)
    putLazyByteString v
  put (Int32 v)  = do
    putWord8 2
    putWord32be (fromIntegral v)
  put (UInt32 v) = do
    putWord8 3
    putWord32be v
  put (Int64 v)  = do
    putWord8 4
    putWord64be (fromIntegral v)
  put (UInt64 v) = do
    putWord8 5
    putWord64be v
  put (Double v) = do
    putWord8 6
    putFloat64be v

  get = do
    magic <- getWord8
    case magic of
      0 -> fmap (Bool . toEnum . fromIntegral) getWord8
      1 -> fmap Text (getWord16be >>= getLazyByteString . fromIntegral)
      2 -> fmap (Int32 . fromIntegral) getWord32be
      3 -> fmap UInt32 getWord32be
      4 -> fmap (Int64 . fromIntegral) getWord64be
      5 -> fmap UInt64 getWord64be
      6 -> fmap Double getFloat64be
      _ -> throw (SystemExcept (Just "Types/get: error decoding Value type"))

instance Functor Mode where

  fmap f (All ma)     = All (fmap f ma)
  fmap f (Prefix a b) = Prefix (f a) (f b)
  fmap f (Precise a)  = Precise (f a)

instance AsLazyByteString User where

  asLazyByteString (User u) = u

instance AsLazyByteString Tree where

  asLazyByteString (Tree t) = t

instance AsLazyByteString GUID where

  asLazyByteString (GUID g) = g

instance AsLazyByteString Label where

  asLazyByteString (Label l) = l

instance AsLazyByteString Kind where

  asLazyByteString (Kind k) = k

instance AsLazyByteString Node where

  asLazyByteString (Node n) = n

instance AsLazyByteString Attr where

  asLazyByteString (Attr a) = a

instance NFData Value where

  rnf (Bool v)   = rnf v
  rnf (Text v)   = rnf v
  rnf (Double v) = rnf v
  rnf (Int32 v)  = rnf v
  rnf (Int64 v)  = rnf v
  rnf (UInt32 v) = rnf v
  rnf (UInt64 v) = rnf v

instance Hashable GUID where

  hashWithSalt salt (GUID v) = hashWithSalt salt v

instance Hashable Label where

  hashWithSalt salt (Label v) = hashWithSalt salt v

instance Hashable Node where

  hashWithSalt salt (Node v) = hashWithSalt salt v

instance Hashable User where

  hashWithSalt salt (User v) = hashWithSalt salt v

instance Hashable Tree where

  hashWithSalt salt (Tree v) = hashWithSalt salt v

instance Hashable Kind where

  hashWithSalt salt (Kind v) = hashWithSalt salt v

instance Hashable Attr where

  hashWithSalt salt (Attr v) = hashWithSalt salt v
