{-# LANGUAGE FlexibleInstances #-}

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

module Leela.Data.LQL.Lang
    ( Document ()
    , link
    , using
    , match
    , deref
    , create
    , (.>)
    , (..>)
    , (<$>)
    , render
    ) where

import           Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy.Builder

data Document = Document Builder

class ToBuilder a where

  build :: a -> Builder

using :: (ToBuilder user, ToBuilder tree) => user -> tree -> Document
using user tree = Document $
  string7 "using "
  <> build user
  <> char7 ' '
  <> build tree

create :: (ToBuilder a) => a -> Document
create a = Document (string7 "create " <> build a)

link :: (ToBuilder a, ToBuilder l, ToBuilder b) => a -> l -> b -> Document
link a l b = Document $
  string7 "create "
  <> build a
  <> string7 " -"
  <> build l
  <> string7 "> "
  <> build b

match :: (ToBuilder node) => node -> Document
match node = Document (string7 "match " <> build node)

deref :: (ToBuilder guid) => guid -> Document
deref guid = Document (string7 "deref " <> build guid)

(.>) :: (ToBuilder label) => Document -> label -> Document
(.>) doc label = doc ..> (label, "()")

(..>) :: (ToBuilder label, ToBuilder node) => Document -> (label, node) -> Document
(..>) (Document m) (label, node) = Document $
  m <> string7 " -"
  <> build label
  <> string7 "> "
  <> build node

(<$>) :: Document -> Document -> Document
(<$>) (Document l) (Document r) = Document (l <> char7 '\n' <> r)

render :: Document -> L.ByteString
render (Document doc) = toLazyByteString (doc <> char7 ';')

instance ToBuilder [Char] where

  build = stringUtf8

instance ToBuilder L.ByteString where

  build = lazyByteString

instance ToBuilder B.ByteString where

  build = byteString
