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
              | Config
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
  mapM_ (flip setupLog (setLevel prio)) [HZMQ, Global, Config, Network, Storage]

facility :: Facility -> String
facility HZMQ    = "leela.hzmq"
facility Global  = "leela"
facility Config  = "leela.config"
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
