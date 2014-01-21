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

module Leela.Data.Naming
       ( GUID (..)
       , Node (..)
       , Tree (..)
       , User (..)
       , Label (..)
       , AsByteString (..)
       ) where

import Data.ByteString as B

newtype GUID = GUID B.ByteString
        deriving (Eq, Ord, Show)

newtype Label = Label B.ByteString
        deriving (Eq, Ord, Show)

newtype Node = Node B.ByteString
        deriving (Eq, Ord, Show)

newtype User = User B.ByteString
        deriving (Eq, Ord, Show)

newtype Tree = Tree B.ByteString
        deriving (Eq, Ord, Show)

newtype Attr = Attr B.ByteString
        deriving (Eq, Ord, Show)

class AsByteString a where

  toByteString :: a -> B.ByteString

instance AsByteString GUID where

  toByteString (GUID g) = g

instance AsByteString Label where

  toByteString (Label l) = l

instance AsByteString Node where

  toByteString (Node n) = n

instance AsByteString User where

  toByteString (User u) = u

instance AsByteString Tree where

  toByteString (Tree t) = t
