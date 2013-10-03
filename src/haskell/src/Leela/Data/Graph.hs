{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

data Result r = Load (Matcher (Result r)) (Result r)
              | Done r [Journal]
              | Fail String

data Cursor = Head GUID
            | Item [(GUID, Label)] [(GUID, Label)] Cursor
            | Need (Result Cursor)
            | Tail

done :: a -> Result a
done a = Done a []

loadNode :: GUID -> Maybe Label -> ([(GUID, Label)] -> Result r) -> Result r -> Result r
loadNode k Nothing f  = Load (ByNode k f)
loadNode k (Just l) f = Load (ByLabel k l f)

loadNode1 :: GUID -> Maybe Label -> ([(GUID, Label)] -> Result r) -> Result r
loadNode1 k l f = loadNode k l f (fail "not found")

putNode :: Namespace -> Key -> Result ()
putNode n k = Done () [PutNode n k (guid $ derive n k)]

start :: GUID -> Cursor
start g = Head g

select :: Label -> (GUID -> Bool) -> Cursor -> Cursor
select _ _ Tail                             = Tail
select wantl f (Head a)                     = Need $
  loadNode1 a (Just wantl) $ \nodes -> done $
    Item [] (filter (f . fst) nodes) Tail
select wantl f (Need r)                          = Need (r >>= done . select wantl f)
select wantl f (Item _ [] cont)                  = select wantl f cont
select wantl f (Item path (item@(b, _):xs) cont) = Need $
  loadNode1 b (Just wantl) $ \nodes -> done $
    Item (item:path) (filter (f . fst) nodes) (select wantl f (Item path xs cont))

putLink :: Link -> Result ()
putLink lnk = putLinks [lnk]

putLinks :: [Link] -> Result ()
putLinks lnks = Done () [ PutLabel $ map (\(a, _, l) -> (a, l)) lnks,
                          PutLink $ map (\(a, b, l) -> (labelRef a l, b)) lnks
                        ]

labelRef :: GUID -> Label -> GUID
labelRef a l = rehash a $ unpack l

bindWith :: ([Journal] -> [Journal] -> [Journal]) -> Result r1 -> (r1 -> Result r) -> Result r
bindWith _ (Fail s) _                     = Fail s
bindWith merge (Load (ByLabel k l f) g) h = Load (ByLabel k l (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Load (ByNode k f) g) h  = Load (ByNode k (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Done r j) f               = mergeLog (f r)
    where
      mergeLog (Done r1 j1)             = Done r1 (j `merge` j1)
      mergeLog (Fail s)                 = Fail s
      mergeLog (Load (ByLabel k l g) h) = Load (ByLabel k l (\v -> mergeLog (g v))) (mergeLog h)
      mergeLog (Load (ByNode k g) h)    = Load (ByNode k (\v -> mergeLog (g v))) (mergeLog h)

bindAndLog :: Result r1 -> (r1 -> Result r) -> Result r
bindAndLog = bindWith (++)

bindNoLog :: Result r1 -> (r1 -> Result r) -> Result r
bindNoLog = bindWith f
    where
      f [] [] = []
      f _ _   = error "bindNoLog: not empty"

fmapR :: (r1 -> r) -> Result r1 -> Result r
fmapR _ (Fail s)                 = Fail s
fmapR f (Load (ByLabel k l g) h) = Load (ByLabel k l (\v -> fmapR f (g v))) (fmapR f h)
fmapR f (Load (ByNode k g) h)    = Load (ByNode k (\v -> fmapR f (g v))) (fmapR f h)
fmapR f (Done r j)               = Done (f r) j

instance Monad Result where

  fail s   = Fail s
  return a = done a
  f >>= g  = f `bindAndLog` g

instance Functor Result where

  fmap = fmapR
