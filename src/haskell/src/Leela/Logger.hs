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

module Leela.Logger
       ( Facility (..)
       , Priority (..)
       , fmt
       , linfo
       , lwarn
       , ldebug
       , lerror
       , printf
       , lnotice
       , logsetup
       , setupLog
       , lcritical
       ) where

import System.IO
import Text.Printf
import Data.ByteString (ByteString)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Log.Handler.Simple
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Lazy.Builder

data Facility = HZMQ
              | Global
              | Types
              | Network
              | Storage

class ToString a where
    fmt :: a -> String

myfmt :: LogFormatter a
myfmt = simpleLogFormatter "[$utcTime] [$prio/$loggername.$pid] $msg"

logsetup :: Priority -> IO ()
logsetup prio = do
  h <- fmap (flip setFormatter myfmt) (streamHandler stderr prio)
  updateGlobalLogger rootLoggerName (setHandlers [h])
  mapM_ (flip setupLog (setLevel prio)) [HZMQ, Global, Types, Network, Storage]

facility :: Facility -> String
facility HZMQ    = "leela.hzmq"
facility Global  = "leela"
facility Types  = "leela.naming"
facility Network = "leela.network"
facility Storage = "leela.storage"

setupLog :: Facility -> (Logger -> Logger) -> IO ()
setupLog sys = updateGlobalLogger (facility sys)

ldebug :: Facility -> String -> IO ()
ldebug sys = debugM (facility sys)

linfo :: Facility -> String -> IO ()
linfo sys = infoM (facility sys)

lnotice :: Facility -> String -> IO ()
lnotice sys = noticeM (facility sys)

lwarn :: Facility -> String -> IO ()
lwarn sys = warningM (facility sys) . fmt

lerror :: Facility -> String -> IO ()
lerror sys = errorM (facility sys)

lcritical :: Facility -> String -> IO ()
lcritical sys = criticalM (facility sys) . fmt

instance ToString ByteString where

  fmt = fmt . toString . toLazyByteString . byteString

instance ToString Double where

  fmt = show

instance ToString Int where

  fmt = show

instance ToString Char where

  fmt '\n' = "[\\n]"
  fmt c    = [c]

instance (ToString a) => ToString [a] where

  fmt = foldr (\a acc -> fmt a ++ acc) ""

instance (ToString a) => ToString (Maybe a) where

  fmt Nothing  = "<<nothing>>"
  fmt (Just s) = fmt s
