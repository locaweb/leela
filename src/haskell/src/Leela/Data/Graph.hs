{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

-- | This module contains operations we can perform on our distributed
-- graph. This implementation uses a logging technique to modify the
-- graph and continuations to load stored data. The log entries are
-- designed so that a adjacency list representation is easy to
-- implement.
--
-- Special care must be taken when implementing the storage
-- engine. Implementations must adhere to the following memory model:
--
--   * read-after-write between two different Result types must never fail;
--   * read-after-write within a Result type may fail;
--   * writes within a Result can happen out-of-order;
--   * writes between two different Results must never happen out-of-order;
--
-- To make this clear, the following likely wont work:
--
-- > (Done () [PutNode n k]) >>= (Load k f g)
--
-- To accomplish this you must first write the node (letting the
-- storage engine finish its business) and create another Result type
-- with the Load request.
module Leela.Data.Graph
    ( Result (..)
    , Matcher (..)
    , Cursor (..)
    , Link
    -- * Modifying
    , putLinks
    , putNode
    , putLink
    -- * Creating
    , done
    , loadNode
    , loadNode1
    -- * Querying
    , start
    , select
    -- * Binding
    , bindAndLog
    , bindNoLog
    -- * Misc
    , labelRef
    ) where

import Leela.Data.Journal
import Leela.Data.Namespace

type Link = (GUID, GUID, Label)

data Matcher r = ByLabel GUID Label ([(GUID, Label)] -> r)
               | ByNode GUID ([(GUID, Label)] -> r)
               | ByEdge GUID Label GUID ([(GUID, Label)] -> r)

data Result r = Load (Matcher (Result r)) (Result r)
              | Done r [Journal]
              | Fail Int String

data Cursor = Head GUID
            | Item [(GUID, Label)] [(GUID, Label)] Cursor
            | Need (Result Cursor)
            | Tail

done :: a -> Result a
done a = Done a []

loadNode :: GUID -> Maybe Label -> Maybe GUID -> ([(GUID, Label)] -> Result r) -> Result r -> Result r
loadNode a Nothing Nothing f   = Load (ByNode a f)
loadNode a (Just l) Nothing f  = Load (ByLabel a l f)
loadNode a Nothing (Just b) f  = Load (ByNode a (f . filter ((== b) . fst)))
loadNode a (Just l) (Just b) f = Load (ByEdge a l b f)

loadNode1 :: GUID -> Maybe Label -> Maybe GUID -> ([(GUID, Label)] -> Result r) -> Result r
loadNode1 a l b f = loadNode a l b f (Fail 404 "not found")

putNode :: Namespace -> Key -> Result ()
putNode n k = Done () [PutNode n k (guid $ derive n k)]

start :: GUID -> Cursor
start = Head

select :: Label -> Maybe GUID -> Cursor -> Cursor
select _ _ Tail                                 = Tail
select wantl wantb (Head a)                     =
  let item nodes = Item [] nodes Tail
      onSucc     = done . item
      onFail     = done Tail
  in Need $ loadNode a (Just wantl) wantb onSucc onFail
select wantl wantb (Need r)                     = Need (r >>= done . select wantl wantb)
select wantl wantb (Item _ [] cont)             = select wantl wantb cont
select wantl wantb (Item path ((b, l):xs) cont) =
  let item nodes = Item ((b, l):path) nodes (select wantl wantb (Item path xs cont))
      onSucc     = done . item
      onFail     = done $ select wantl wantb (Item path xs cont)
  in Need $ loadNode b (Just wantl) wantb onSucc onFail

putLink :: Link -> Result ()
putLink lnk = putLinks [lnk]

putLinks :: [Link] -> Result ()
putLinks = Done () . concatMap (\(a, b, l) -> [PutLabel a [l], PutLink (labelRef a l) [b]])

labelRef :: GUID -> Label -> GUID
labelRef a l = rehash a $ unpack l

bindWith :: ([Journal] -> [Journal] -> [Journal]) -> Result r1 -> (r1 -> Result r) -> Result r
bindWith _ (Fail c s) _                     = Fail c s
bindWith merge (Load (ByLabel k l f) g) h   = Load (ByLabel k l (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Load (ByNode k f) g) h      = Load (ByNode k (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Load (ByEdge a l b f) g) h  = Load (ByEdge a l b (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Done r j) f                 = mergeLog (f r)
    where
      mergeLog (Done r1 j1)              = Done r1 (j `merge` j1)
      mergeLog (Fail c s)                = Fail c s
      mergeLog (Load (ByLabel k l g) h)  = Load (ByLabel k l (mergeLog . g)) (mergeLog h)
      mergeLog (Load (ByNode k g) h)     = Load (ByNode k (mergeLog . g)) (mergeLog h)
      mergeLog (Load (ByEdge a l b g) h) = Load (ByEdge a l b (mergeLog . g)) (mergeLog h)

bindAndLog :: Result r1 -> (r1 -> Result r) -> Result r
bindAndLog = bindWith (\j0 j1 -> rechunk (j0 ++ j1))

bindNoLog :: Result r1 -> (r1 -> Result r) -> Result r
bindNoLog = bindWith f
    where
      f [] [] = []
      f _ _   = error "bindNoLog: not empty"

fmapR :: (r1 -> r) -> Result r1 -> Result r
fmapR _ (Fail c s)                = Fail c s
fmapR f (Load (ByLabel k l g) h)  = Load (ByLabel k l (fmapR f . g)) (fmapR f h)
fmapR f (Load (ByNode k g) h)     = Load (ByNode k (fmapR f . g)) (fmapR f h)
fmapR f (Load (ByEdge a l b g) h) = Load (ByEdge a l b (fmapR f . g)) (fmapR f h)
fmapR f (Done r j)                = Done (f r) j

instance Monad Result where

  return = done

  fail = Fail 500

  f >>= g  = f `bindAndLog` g

instance Functor Result where

  fmap = fmapR
