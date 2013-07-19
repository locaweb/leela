{-# LANGUAGE OverloadedStrings #-}

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
    , Context (..)
    , Cursor (..)
    , Link
    -- * Modifying
    , putLinks
    , putNode
    , putLink
    -- * Creating
    , done
    , context
    , loadNode
    , loadNode1
    -- * Querying
    , outwith
    , select
    , outlabelWith
    , outnodeWith
    , outlabel
    , outnode
    -- * Binding
    , bindAndLog
    , bindNoLog
    ) where

import Data.Aeson hiding (Result(..))
import Leela.Data.Journal
import Leela.Data.Namespace

type Link = (GUID, GUID, Label)

newtype Context = Context (GUID, [(GUID, Label)])

data Matcher r = ByNode GUID (Context -> r)

data Result r = Load (Matcher (Result r)) (Result r)
              | Done r [Journal]
              | Fail String

data Cursor = ItemL [Either GUID Label] Link Cursor
            | ItemK [Either GUID Label] Cursor
            | Need (Result Cursor)
            | EOF

done :: a -> Result a
done a = Done a []

context :: GUID -> [(GUID, Label)] -> Context
context a b = Context (a, b)

loadNode :: GUID -> (Context -> Result r) -> Result r -> Result r
loadNode k f = Load (ByNode k f)

loadNode1 :: GUID -> (Context -> Result r) -> Result r
loadNode1 k f = loadNode k f (fail "not found")

-- loadNodeL :: Namespace -> Key -> Label -> (Context -> Result r) -> Result r -> Result r
-- loadNodeL n k l f = Load (ByLabel n k l f)

-- loadNodeL1 :: Namespace -> Key -> Label -> (Context -> Result r) -> Result r
-- loadNodeL1 n k l f = loadNode n k l f (fail "not found")

putNode :: Namespace -> Key -> Result ()
putNode n k = Done () [PutNode n k (guid $ derive n k)]

asCursor :: [Either GUID Label] -> Cursor -> Context -> Cursor
asCursor path cont (Context (k, []))    = ItemK (Left k : path) cont
asCursor path cont (Context (k, edges)) = let ([(b, l)], rest) = splitAt 1 edges
                                          in ItemL (Right l : Left k : path) (k, b, l) (asCursor path cont (Context (k, rest)))

select :: GUID -> Cursor
select k = Need (loadNode1 k (done . asCursor [] EOF))

outlabel :: Label -> Cursor -> Cursor
outlabel l0 = outlabelWith (l0 ==)

outlabelWith :: (Label -> Bool) -> Cursor -> Cursor
outlabelWith p = outwith (\(_, _, l) -> p l)

outnode :: Label -> GUID -> Cursor -> Cursor
outnode l0 k0 = outnodeWith (l0 ==) (k0 ==)

outnodeWith :: (Label -> Bool) -> (GUID -> Bool) -> Cursor -> Cursor
outnodeWith pl pk = outwith (\(_, k, l) -> pk k && pl l)

outwith :: (Link -> Bool) -> Cursor -> Cursor
outwith f (ItemK _ cursor)
                      = outwith f cursor
outwith f (ItemL path l@(_, k, _) cursor)
    | f l             = Need (loadNode1 k (done . asCursor path (outwith f cursor)))
    | otherwise       = outwith f cursor
outwith _ EOF         = EOF
outwith f (Need result)
                      = Need (result >>= done . outwith f)

putLink :: Link -> Result ()
putLink (a, b, l) = Done () [PutLink [(a, b, l)]]

putLinks :: [Link] -> Result ()
putLinks lnks = Done () [PutLink lnks]

bindWith :: ([Journal] -> [Journal] -> [Journal]) -> Result r1 -> (r1 -> Result r) -> Result r
bindWith _ (Fail s) _                    = Fail s
bindWith merge (Load (ByNode k f) g) h   = Load (ByNode k (\ctx -> bindWith merge (f ctx) h)) (bindWith merge g h)
bindWith merge (Done r j) f              = mergeLog (f r)
    where mergeLog (Done r1 j1)            = Done r1 (j `merge` j1)
          mergeLog (Fail s)                = Fail s
          mergeLog (Load (ByNode k g) h)   = Load (ByNode k (\ctx -> mergeLog (g ctx))) (mergeLog h)

bindAndLog :: Result r1 -> (r1 -> Result r) -> Result r
bindAndLog = bindWith (\a b -> jmerge (a ++ b))

bindNoLog :: Result r1 -> (r1 -> Result r) -> Result r
bindNoLog = bindWith f
    where f [] [] = []
          f _ _   = error "bindNoLog: not empty"

-- | Apply a function over the Done result.
fmapR :: (r1 -> r) -> Result r1 -> Result r
fmapR _ (Fail s)                = Fail s
fmapR f (Load (ByNode k g) h)   = Load (ByNode k (\ctx -> fmapR f (g ctx))) (fmapR f h)
fmapR f (Done r j)              = Done (f r) j

instance Monad Result where

    fail s   = Fail s
    return a = done a
    f >>= g  = f `bindAndLog` g

instance Functor Result where

    fmap = fmapR

instance ToJSON Context where

    toJSON (Context (k, links)) = object [ ("node", toJSON k)
                                         , ("edges", toJSON links)
                                         ]
