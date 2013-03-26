-- -*- mode: haskell; -*-
-- All Rights Reserved.
--
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
--
--        http://www.apache.org/licenses/LICENSE-2.0
--
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.

module DarkMatter.Logger
       ( Priority(DEBUG, INFO, WARNING, ERROR)
       , setlevel
       , debug
       , info
       , warn
       , crit
       )  where

import System.Log
import System.Log.Logger
import System.Time
import System.Locale

myname :: String
myname = "darkmatter"

setlevel :: Priority -> IO ()
setlevel p = updateGlobalLogger myname (setLevel p)

logfmt :: String -> IO String
logfmt s = do { when <- datetime
              ; return ("[darkmatter|" ++ when ++ "] " ++ s)
              }
  where datetime = do { clock    <- getClockTime
                      ; calendar <- toCalendarTime clock
                      ; return (formatCalendarTime defaultTimeLocale "%Y%m%dT%H%M%s%S %Z" calendar)
                      }

debug :: String -> IO ()
debug s = logfmt s >>= debugM myname

info :: String -> IO ()
info s = logfmt s >>= infoM myname

warn :: String -> IO ()
warn s = logfmt s >>= warningM myname

crit :: String -> IO ()
crit s = logfmt s >>= errorM myname

