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

import           Control.Exception
import           Leela.Data.QDevice
import qualified Data.ByteString.Lazy as L
import           Leela.Data.Namespace

data AnyBackend = forall b. (GraphBackend b) => AnyBackend { anyBackend :: b }

data Mode a = All (Maybe a)
            | Prefix a a
            | Suffix a a
            | Precise a

type Page = Int

pageSize :: Int
pageSize = 512

class GraphBackend m where

  getName  :: GUID -> m -> IO (Namespace, Key)

  putName  :: Namespace -> Key -> GUID -> m -> IO ()

  getEdge  :: Device (Either SomeException (Page, [(GUID, GUID)])) -> [(GUID, GUID)] -> m -> IO ()

  getLink  :: Device (Either SomeException (Page, [(GUID, GUID)])) -> [GUID] -> m -> IO ()

  putLink  :: GUID -> [GUID] -> m -> IO ()

  getLabel :: Device (Either SomeException (Page, [Label])) -> GUID -> (Mode Label) -> m -> IO ()

  putLabel :: GUID -> [Label] -> m -> IO ()

  unlink   :: GUID -> Maybe GUID -> m -> IO ()

glob :: Label -> Mode Label
glob l
  | "*" == s           = All Nothing
  | L.isPrefixOf "*" s = uncurry Suffix (range $ L.tail s)
  | L.isSuffixOf "*" s = uncurry Prefix (range $ L.init s)
  | otherwise          = Precise l
    where s :: L.ByteString
          s = unpack l

          range str = (pack str, pack $ L.init str `L.snoc` (L.last str + 1))

nextPage :: Mode Label -> Label -> Maybe (Mode Label)
nextPage (All _) l      = Just $ All (Just l)
nextPage (Prefix _ b) l = Just $ Prefix l b
nextPage (Suffix _ b) l = Just $ Suffix l b
nextPage _ _            = Nothing

instance GraphBackend AnyBackend where

  getName g (AnyBackend b) = getName g b

  putName n k g (AnyBackend b) = putName n k g b

  getLabel dev g m (AnyBackend b) = getLabel dev g m b

  putLabel g lbls (AnyBackend b) = putLabel g lbls b

  getEdge dev gs (AnyBackend b) = getEdge dev gs b

  getLink dev g (AnyBackend b) = getLink dev g b

  putLink g lnks (AnyBackend b) = putLink g lnks b

  unlink a mb (AnyBackend b) = unlink a mb b
