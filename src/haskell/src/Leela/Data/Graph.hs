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
    , GUID (..)
    , Link
    -- * Modifying
    , unlinkAll
    , putLinks
    , putNode
    , putLink
    , delete
    , unlink
    -- * Creating
    , done
    , loadNode
    , loadNode1
    -- * Querying
    , start
    , select
    -- * Binding
    , bind
    ) where

import Leela.Data.Journal
import Leela.Data.Naming

type Link = (GUID, GUID, Label)

data Matcher r = ByLabel GUID Label ([(GUID, Label)] -> r)
               | ByNode GUID ([(GUID, Label)] -> r)
               | ByEdge GUID Label GUID ([(GUID, Label)] -> r)

data Result r = Load (Matcher (Result r)) (Result r)
              | Done r
              | Fail Int String

data Cursor = Head GUID
            | Item [(GUID, Label)] [(GUID, Label)] Cursor
            | Need (Result Cursor)
            | Tail

done :: a -> Result a
done a = Done a

loadNode :: GUID -> Maybe Label -> Maybe GUID -> ([(GUID, Label)] -> Result r) -> Result r -> Result r
loadNode a Nothing Nothing f   = Load (ByNode a f)
loadNode a (Just l) Nothing f  = Load (ByLabel a l f)
loadNode a Nothing (Just b) f  = Load (ByNode a (f . filter ((== b) . fst)))
loadNode a (Just l) (Just b) f = Load (ByEdge a l b f)

loadNode1 :: GUID -> Maybe Label -> Maybe GUID -> ([(GUID, Label)] -> Result r) -> Result r
loadNode1 a l b f = loadNode a l b f (Fail 404 "not found")

putNode :: User -> Tree -> Node -> [Journal]
putNode u t k = [PutNode u t k]

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

unlink :: Link -> [Journal]
unlink (a, b, l) = [DelLink a (Just b) l]

unlinkAll :: (GUID, Label) -> [Journal]
unlinkAll (a, l) = [DelLink a Nothing l]

delete :: GUID -> [Journal]
delete a = [DelNode a]

putLink :: Link -> [Journal]
putLink lnk = putLinks [lnk]

putLinks :: [Link] -> [Journal]
putLinks []             = []
putLinks ((a, b, l):xs) = PutLabel a l : PutLink a b l : putLinks xs

bind :: Result r1 -> (r1 -> Result r) -> Result r
bind (Fail c s) _                     = Fail c s
bind (Load (ByLabel k l f) g) h   = Load (ByLabel k l (\v -> bind (f v) h)) (bind g h)
bind (Load (ByNode k f) g) h      = Load (ByNode k (\v -> bind (f v) h)) (bind g h)
bind (Load (ByEdge a l b f) g) h  = Load (ByEdge a l b (\v -> bind (f v) h)) (bind g h)
bind (Done r) f                   = f r

fmapR :: (r1 -> r) -> Result r1 -> Result r
fmapR _ (Fail c s)                = Fail c s
fmapR f (Load (ByLabel k l g) h)  = Load (ByLabel k l (fmapR f . g)) (fmapR f h)
fmapR f (Load (ByNode k g) h)     = Load (ByNode k (fmapR f . g)) (fmapR f h)
fmapR f (Load (ByEdge a l b g) h) = Load (ByEdge a l b (fmapR f . g)) (fmapR f h)
fmapR f (Done r)                  = Done (f r)

instance Monad Result where

  return = done

  fail = Fail 500

  f >>= g  = f `bind` g

instance Functor Result where

  fmap = fmapR
