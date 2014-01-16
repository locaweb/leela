{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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

module Leela.Storage.Backend
    ( Mode (..)
    , AnyBackend (..)
    , GraphBackend (..)
    , glob
    , nextPage
    , pageSize
    ) where

import qualified Data.ByteString as B
import           Control.Exception
import           Leela.Data.Naming
import           Leela.Data.QDevice

data AnyBackend = forall b. (GraphBackend b) => AnyBackend { anyBackend :: b }

data Mode a = All (Maybe a)
            | Prefix a a
            | Suffix a a
            | Precise a

type Page = Int

pageSize :: Int
pageSize = 128

class GraphBackend m where

  getName  :: GUID -> m -> IO (User, Tree, Node)

  getGUID  :: User -> Tree -> Node -> m -> IO (Maybe GUID)

  putName  :: User -> Tree -> Node -> m -> IO GUID

  hasLink  :: GUID -> Label -> GUID -> m -> IO Bool

  getLink  :: Device (Either SomeException (Page, [GUID])) -> GUID -> Label -> m -> IO ()

  putLink  :: GUID -> Label -> GUID -> m -> IO ()

  getLabel :: Device (Either SomeException (Page, [Label])) -> GUID -> Mode Label -> m -> IO ()

  putLabel :: GUID -> Label -> m -> IO ()

  unlink   :: GUID -> Label -> Maybe GUID -> m -> IO ()

  delete   :: GUID -> m -> IO ()

glob :: Label -> Mode Label
glob l@(Label s)
  | "*" == s           = All Nothing
  | B.isPrefixOf "*" s = uncurry Suffix (range $ B.tail s)
  | B.isSuffixOf "*" s = uncurry Prefix (range $ B.init s)
  | otherwise          = Precise l
    where
      range str = (Label str, Label $ B.init str `B.snoc` (B.last str + 1))

nextPage :: Mode Label -> Label -> Maybe (Mode Label)
nextPage (All _) l      = Just $ All (Just l)
nextPage (Prefix _ b) l = Just $ Prefix l b
nextPage (Suffix _ b) l = Just $ Suffix l b
nextPage _ _            = Nothing

instance GraphBackend AnyBackend where

  getName g (AnyBackend db) = getName g db

  getGUID u t n (AnyBackend db) = getGUID u t n db

  putName u t n (AnyBackend db) = putName u t n db

  getLabel dev g m (AnyBackend db) = getLabel dev g m db

  putLabel g l (AnyBackend db) = putLabel g l db

  hasLink a l b (AnyBackend db) = hasLink a l b db

  getLink dev a l (AnyBackend db) = getLink dev a l db

  putLink a l b (AnyBackend db) = putLink a l b db

  unlink a l mb (AnyBackend db) = unlink a l mb db

  delete a (AnyBackend db) = delete a db
