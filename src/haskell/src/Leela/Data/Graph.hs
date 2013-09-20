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
    , loadLabel
    , loadLabel1
    -- * Querying
    , start
    , select
    -- * Binding
    , bindAndLog
    , bindNoLog
    ) where

import Leela.Data.Journal
import Leela.Data.Namespace

type Link = (GUID, GUID, Label)

data Matcher r = ByNode GUID ([Label] -> r)
               | ByLabel GUID ([GUID] -> r)

data Result r = Load (Matcher (Result r)) (Result r)
              | Done r [Journal]
              | Fail String

data Cursor = Item [(GUID, Label)] [GUID] Cursor
            | Need (Result Cursor)
            | EOF

done :: a -> Result a
done a = Done a []

loadNode :: GUID -> ([Label] -> Result r) -> Result r -> Result r
loadNode k f = Load (ByNode k f)

loadNode1 :: GUID -> ([Label] -> Result r) -> Result r
loadNode1 k f = loadNode k f (fail "not found")

loadLabel :: GUID -> Label -> ([GUID] -> Result r) -> Result r -> Result r
loadLabel g l f = Load (ByLabel (rehash g $ unpack l) f)

loadLabel1 :: GUID -> Label -> ([GUID] -> Result r) -> Result r
loadLabel1 g l f = loadLabel g l f (fail "not found")

putNode :: Namespace -> Key -> Result ()
putNode n k = Done () [PutNode n k (guid $ derive n k)]

start :: GUID -> Cursor
start g = Item [] [g] EOF

select :: (Label -> Bool) -> (GUID -> Bool) -> Cursor -> Cursor
select _ _ EOF                     = EOF
select f g (Need r)                = Need (r >>= done . select f g)
select f g (Item _ [] cont)        = select f g cont
select f g (Item path (x:xs) cont) = Need (loadNode1 x matchAndLoad)
    where
      matchAndLoad [] = done (select f g $ Item path xs cont)
      matchAndLoad (y:ys)
        | f y       = loadLabel1 x y (\nodes -> done $ Item ((x,y):path) (filter g nodes) (Need (matchAndLoad ys)))
        | otherwise = matchAndLoad ys

putLink :: Link -> Result ()
putLink lnk = putLinks [lnk]

putLinks :: [Link] -> Result ()
putLinks lnks = Done () [ PutLabel $ map (\(a, _, l) -> (a, l)) lnks,
                          PutLink $ map (\(a, b, l) -> (rehash a $ unpack l, b)) lnks
                        ]

bindWith :: ([Journal] -> [Journal] -> [Journal]) -> Result r1 -> (r1 -> Result r) -> Result r
bindWith _ (Fail s) _                   = Fail s
bindWith merge (Load (ByNode k f) g) h  = Load (ByNode k (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Load (ByLabel k f) g) h = Load (ByLabel k (\v -> bindWith merge (f v) h)) (bindWith merge g h)
bindWith merge (Done r j) f             = mergeLog (f r)
    where
      mergeLog (Done r1 j1)           = Done r1 (j `merge` j1)
      mergeLog (Fail s)               = Fail s
      mergeLog (Load (ByNode k g) h)  = Load (ByNode k (\v -> mergeLog (g v))) (mergeLog h)
      mergeLog (Load (ByLabel k g) h) = Load (ByLabel k (\v -> mergeLog (g v))) (mergeLog h)

bindAndLog :: Result r1 -> (r1 -> Result r) -> Result r
bindAndLog = bindWith (++)

bindNoLog :: Result r1 -> (r1 -> Result r) -> Result r
bindNoLog = bindWith f
    where
      f [] [] = []
      f _ _   = error "bindNoLog: not empty"

fmapR :: (r1 -> r) -> Result r1 -> Result r
fmapR _ (Fail s)               = Fail s
fmapR f (Load (ByNode k g) h)  = Load (ByNode k (\v -> fmapR f (g v))) (fmapR f h)
fmapR f (Load (ByLabel k g) h) = Load (ByLabel k (\v -> fmapR f (g v))) (fmapR f h)
fmapR f (Done r j)             = Done (f r) j

instance Monad Result where

  fail s   = Fail s
  return a = done a
  f >>= g  = f `bindAndLog` g

instance Functor Result where

  fmap = fmapR
