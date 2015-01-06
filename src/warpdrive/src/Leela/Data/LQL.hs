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

module Leela.Data.LQL
    ( LQL (..)
    , Grep (..)
    , Using (..)
    , AttrEvent (..)
    , GraphEvent (..)
    , grep
    , grepID
    , isStat
    , targetUser
    , lqlDescr
    , groupLQL
    ) where

import           Data.Maybe
import           Crypto.Hash
import qualified Data.Vector as V
import qualified Data.Sequence as S
import           Data.Foldable (toList)
import           Data.Serialize
import qualified Data.Map.Strict as M
import           Leela.Data.Time
import           Text.Regex.TDFA
import           Leela.Data.Types
import           Leela.Data.Pipeline
import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import           Control.Monad.Identity

targetUser :: Using -> User
targetUser u = fromMaybe (uUser u) (uAsUser u)

data Using = Using { uUser   :: User
                   , uTree   :: Tree
                   , uAsUser :: Maybe User
                   }
           deriving (Eq)

data Grep = GrepTAttr (Maybe GUID) Attr
          | GrepKAttr (Maybe GUID) Attr
          | GrepMakeLink (Maybe GUID) Label (Maybe GUID)
          | GrepKillLink (Maybe GUID) Label (Maybe GUID)
          | GrepMakeVertex (Maybe User) (Maybe Tree) Kind Node
          | GrepKillVertex (Maybe GUID)

data LQL = StatStmt
         | PathStmt (Matcher, [(Bool, GUID -> Matcher)])
         | KAttrGetStmt GUID Attr [Pipeline Identity (V.Vector (Time, Double))]
         | TAttrGetStmt GUID Attr TimeRange [Pipeline Identity (V.Vector (Time, Double))]
         | KAttrListStmt GUID (Mode Attr)
         | TAttrListStmt GUID (Mode Attr)
         | TAttrLastStmt (Maybe GUID) Attr
         | NameStmt Using (S.Seq GUID)
         | GUIDStmt Using (S.Seq (Kind, Node))
         | AlterStmt (S.Seq Journal)
         | GrepStmt Grep

data GraphEvent = MakeVertexEvent User Tree Kind Node GUID
                | KillVertexEvent GUID
                | MakeLinkEvent GUID Label GUID
                | KillLinkEvent GUID Label (Maybe GUID)
                deriving (Show, Eq)

data AttrEvent = TAttrPutEvent GUID Attr Time Value [Option]
               | KAttrPutEvent GUID Attr Value [Option]
               | TAttrDelEvent GUID Attr (Maybe Time)
               | KAttrDelEvent GUID Attr
               deriving (Show, Eq)

hashlazy' :: L.ByteString -> L.ByteString
hashlazy' = L.fromStrict . digestToHexByteString . hashAlgo
    where
      hashAlgo :: L.ByteString -> Digest SHA1
      hashAlgo = hashlazy

grepID :: Grep -> GUID
grepID grepExpr =
  case grepExpr of
    GrepKillVertex guidQ                                 -> GUID $ hashlazy' $ L.intercalate "0" [ "kill"
                                                                                                 , maybe "*" (\(GUID g) -> g) guidQ
                                                                                                 ]      
    GrepTAttr guidQ (Attr attrQ)                         -> GUID $ hashlazy' $ L.intercalate "0" ["t-attr"
                                                                                                 , maybe "*" (\(GUID g) -> g) guidQ
                                                                                                 , attrQ
                                                                                                 ]      
    GrepKAttr guidQ (Attr attrQ)                         -> GUID $ hashlazy' $ L.intercalate "0" ["k-attr"
                                                                                                 , maybe "*" (\(GUID g) -> g) guidQ
                                                                                                 , attrQ
                                                                                                 ]      
    GrepMakeLink aguidQ (Label labelQ) bguidQ            -> GUID $ hashlazy' $ L.intercalate "0" ["link"
                                                                                                 , maybe "*" (\(GUID g) -> g) aguidQ
                                                                                                 , maybe "*" (\(GUID g) -> g) bguidQ
                                                                                                 , labelQ
                                                                                                 ]      
    GrepKillLink aguidQ (Label labelQ) bguidQ            -> GUID $ hashlazy' $ L.intercalate "0" [ "unlink"
                                                                                                 , maybe "*" (\(GUID g) -> g) aguidQ
                                                                                                 , maybe "*" (\(GUID g) -> g) bguidQ
                                                                                                 , labelQ
                                                                                                 ]      
    GrepMakeVertex userQ treeQ (Kind kindQ) (Node nodeQ) -> GUID $ hashlazy' $ L.intercalate "0" [ "make"
                                                                                                 , maybe "*" (\(User u) -> u) userQ
                                                                                                 , maybe "*" (\(Tree t) -> t) treeQ
                                                                                                 , kindQ
                                                                                                 , nodeQ
                                                                                                 ]      

grep :: Grep -> Either (GraphEvent -> Bool) (AttrEvent -> Bool)
grep (GrepTAttr guidQ (Attr exprQ)) =
  let regex = makeRegex exprQ :: Regex
  in Right $ \e ->
       case e of
         TAttrPutEvent guid (Attr attr) _ _ _ -> and [ maybe True (== guid) guidQ
                                                     , match regex attr
                                                     ]
         _                                    -> False
grep (GrepKAttr guidQ (Attr exprQ)) =
  let regex = makeRegex exprQ :: Regex
  in Right $ \e ->
       case e of
         KAttrPutEvent guid (Attr attr) _ _ -> and [ maybe True (== guid) guidQ
                                                   , match regex attr
                                                   ]
         _                                  -> False
grep (GrepMakeLink aguidQ (Label labelQ) bguidQ) =
  let regex = makeRegex labelQ :: Regex
  in Left $ \e ->
       case e of
         MakeLinkEvent aguid (Label labelV) bguid -> and [ maybe True (aguid ==) aguidQ
                                                         , maybe True (bguid ==) bguidQ
                                                         , match regex labelV
                                                         ]
         _                                        -> False
grep (GrepKillLink aguidQ (Label labelQ) bguidQ) =
  let regex = makeRegex labelQ :: Regex
  in Left $ \e ->
       case e of
         KillLinkEvent aguid (Label labelV) bguid -> and [ maybe True (aguid ==) aguidQ
                                                         , maybe True ((bguid ==) . Just) bguidQ
                                                         , match regex labelV
                                                         ]
         _                                        -> False
grep (GrepMakeVertex userQ treeQ (Kind kindQ) (Node nodeQ)) =
  let kRegex = makeRegex kindQ :: Regex
      nRegex = makeRegex nodeQ :: Regex
  in Left $ \e ->
       case e of
         MakeVertexEvent user tree (Kind kind) (Node node) _ -> and [ maybe True (user ==) userQ
                                                                    , maybe True (tree ==) treeQ
                                                                    , match kRegex kind
                                                                    , match nRegex node
                                                                    ]
         _                                                   -> False
grep (GrepKillVertex guidQ) =
  Left $ \e ->
    case e of
      KillVertexEvent guid -> maybe True (guid ==) guidQ
      _                    -> False

isStat :: LQL -> Bool
isStat StatStmt = True
isStat _        = False

lqlDescr :: [LQL] -> String
lqlDescr = show . go M.empty
    where
      go :: M.Map String Int -> [LQL] -> [(String, Int)]
      go acc []                      = M.toList acc
      go acc (StatStmt : xs)         = go (M.insertWith (+) "stat" 1 acc) xs
      go acc (PathStmt _ : xs)       = go (M.insertWith (+) "path" 1 acc) xs
      go acc (KAttrGetStmt {} : xs)  = go (M.insertWith (+) "attr get(k)" 1 acc) xs
      go acc (KAttrListStmt {} : xs) = go (M.insertWith (+) "attr kls" 1 acc) xs
      go acc (TAttrListStmt {} : xs) = go (M.insertWith (+) "attr tls" 1 acc) xs
      go acc (TAttrGetStmt {} : xs)  = go (M.insertWith (+) "attr get(t)" 1 acc) xs
      go acc (NameStmt {} : xs)      = go (M.insertWith (+) "name" 1 acc) xs
      go acc (GUIDStmt {} : xs)      = go (M.insertWith (+) "guid" 1 acc) xs
      go acc (TAttrLastStmt {} : xs) = go (M.insertWith (+) "attr last" 1 acc) xs
      go acc (GrepStmt {} : xs)      = go (M.insertWith (+) "grep" 1 acc) xs
      go acc (AlterStmt j : xs)      = go (jDescr acc (toList j)) xs

      jDescr acc []                 = acc
      jDescr acc (PutLink {} : xs)  = jDescr (M.insertWith (+) "make(l)" 1 acc) xs
      jDescr acc (PutLabel {} : xs) = jDescr (M.insertWith (+) "make(l)" 1 acc) xs
      jDescr acc (PutNode {} : xs)  = jDescr (M.insertWith (+) "make(n)" 1 acc) xs
      jDescr acc (DelLink {} : xs)  = jDescr (M.insertWith (+) "kill(l)" 1 acc) xs
      jDescr acc (DelNode {} : xs)  = jDescr (M.insertWith (+) "kill(n)" 1 acc) xs
      jDescr acc (DelKAttr {} : xs) = jDescr (M.insertWith (+) "attr kill(k)" 1 acc) xs
      jDescr acc (DelTAttr {} : xs) = jDescr (M.insertWith (+) "attr kill(t)" 1 acc) xs
      jDescr acc (PutKAttr {} : xs) = jDescr (M.insertWith (+) "attr put(k)" 1 acc) xs
      jDescr acc (PutTAttr {} : xs) = jDescr (M.insertWith (+) "attr put(t)" 1 acc) xs

lqlMerge :: LQL -> LQL -> Either LQL (LQL, LQL)
lqlMerge (NameStmt u xs) (NameStmt _ ys) = Left (NameStmt u (xs S.>< ys))
lqlMerge (GUIDStmt u xs) (GUIDStmt _ ys) = Left (GUIDStmt u (xs S.>< ys))
lqlMerge (AlterStmt xs) (AlterStmt ys)   = Left (AlterStmt (xs S.>< ys))
lqlMerge a b                             = Right (a, b)

groupLQL :: [LQL] -> [LQL]
groupLQL = go
    where
      go (a : b : xs) = case (lqlMerge a b) of
                          Left c       -> go (c : xs)
                          Right (c, d) -> c : go (d : xs)
      go xs           = xs

getGraphEvent_v0 :: Get GraphEvent
getGraphEvent_v0 = getWord8 >>= \ty ->
  case ty of
    0 -> MakeVertexEvent <$> get <*> get <*> get <*> get <*> get
    1 -> KillVertexEvent <$> get
    2 -> MakeLinkEvent <$> get <*> get <*> get
    3 -> KillLinkEvent <$> get <*> get <*> get
    _ -> mzero

putGraphEvent_v0 :: GraphEvent -> Put
putGraphEvent_v0 (MakeVertexEvent u t k n g) = do
  putWord8 0
  sequence_ [put u, put t, put k, put n, put g]
putGraphEvent_v0 (KillVertexEvent g) = do
  putWord8 1
  put g
putGraphEvent_v0 (MakeLinkEvent ga l gb) = do
  putWord8 2
  sequence_ [put ga, put l, put gb]
putGraphEvent_v0 (KillLinkEvent ga l mgb) = do
  putWord8 3
  sequence_ [put ga, put l, put mgb]

putAttrEvent_v0 :: AttrEvent -> Put
putAttrEvent_v0 (TAttrPutEvent g a t v opts) = do
  putWord8 0
  sequence_ [put g, put a, put t, put v, put opts]
putAttrEvent_v0 (KAttrPutEvent g a v opts) = do
  putWord8 1
  sequence_ [put g, put a, put v, put opts]
putAttrEvent_v0 (TAttrDelEvent g a mt) = do
  putWord8 2
  sequence_ [put g, put a, put mt]
putAttrEvent_v0 (KAttrDelEvent g a) = do
  putWord8 3
  sequence_ [put g, put a]

getAttrEvent_v0 :: Get AttrEvent
getAttrEvent_v0 = getWord8 >>= \ty ->
  case ty of
    0 -> TAttrPutEvent <$> get <*> get <*> get <*> get <*> get
    1 -> KAttrPutEvent <$> get <*> get <*> get <*> get
    2 -> TAttrDelEvent <$> get <*> get <*> get
    3 -> KAttrDelEvent <$> get <*> get
    _ -> mzero

instance Serialize GraphEvent where

  get   = getWord8 >>= \ver ->
    case ver of
      0 -> getGraphEvent_v0
      _ -> mzero
  put e = putWord8 0 >> putGraphEvent_v0 e

instance Serialize AttrEvent where

  get   = getWord8 >>= \ver ->
    case ver of
      0 -> getAttrEvent_v0
      _ -> mzero
  put e = putWord8 0 >> putAttrEvent_v0 e
