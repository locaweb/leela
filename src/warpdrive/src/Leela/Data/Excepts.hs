{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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

module Leela.Data.Excepts
    ( Excepts (..)
    , isNotFoundExcept
    , isNotFoundExcept'
    ) where

import Data.Typeable
import Control.Exception

data Excepts = BadDeviceExcept (Maybe String)
             | NotFoundExcept (Maybe String)
             | TimeoutExcept (Maybe String)
             | SystemExcept (Maybe String)
             | UserExcept (Maybe String)
             deriving (Show, Typeable, Eq)

instance Exception Excepts

isNotFoundExcept :: Excepts -> Bool
isNotFoundExcept (NotFoundExcept _) = True
isNotFoundExcept _                  = False

isNotFoundExcept' :: SomeException -> Bool
isNotFoundExcept' = maybe False isNotFoundExcept . fromException
