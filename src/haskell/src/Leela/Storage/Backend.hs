{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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

pageSize :: Int
pageSize = 512

class GraphBackend m where

  getName  :: GUID -> m -> IO (Namespace, Key)

  putName  :: Namespace -> Key -> GUID -> m -> IO ()

  getLink  :: Device (Either SomeException [(GUID, GUID)]) -> [GUID] -> m -> IO ()

  putLink  :: GUID -> [GUID] -> m -> IO ()

  getLabel :: Device (Either SomeException [Label]) -> GUID -> (Mode Label) -> m -> IO ()

  putLabel :: GUID -> [Label] -> m -> IO ()

  unlink   :: GUID -> GUID -> m -> IO ()

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

  getLink dev g (AnyBackend b) = getLink dev g b

  putLink g lnks (AnyBackend b) = putLink g lnks b

  unlink a b (AnyBackend be) = unlink a b be
