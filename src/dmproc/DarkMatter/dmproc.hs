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

module Main where

import System.Console.GetOpt
import Control.Concurrent.MVar
import Control.Concurrent
import Network.Socket (SockAddr(..))
import System.Environment
import System.Exit
import System.Posix.Signals
import DarkMatter.Misc
import DarkMatter.Logger
import DarkMatter.Network.Databus
import DarkMatter.Network.Protocol
import DarkMatter.Network.Multicast (attachTo)
import DarkMatter.Network.ProcServer

data OptFlag = Verbose
             deriving (Show)

options :: [OptDescr OptFlag]
options = [ Option "v" ["verbose"] (NoArg Verbose) "increase verbosity"
          ]

getopts :: [String] -> ([OptFlag], String, String, Maybe String)
getopts argv = case (getOpt Permute options argv)
               of (o, [d, s, m], []) -> (o, d, s, Just m)
                  (o, [d, s], [])    -> (o, d, s, Nothing)
                  (_, _, msg)        -> error (concat msg ++ usageInfo header options)
  where header = "USAGE: dmproc [-v|--verbose] DATABUS-FILE SERVER-FILE [PARENT-BROADCAST]"

main :: IO ()
main = do { (opts, sock_dbus, sock_server, sock_mcast) <- fmap getopts getArgs
          ; setopts INFO opts
          ; mutex <- newEmptyMVar
          ; warn "starting server (^C to terminate)"
          ; db    <- newDatabus
          ; _     <- mAttachTo sock_dbus (fmap SockAddrUnix sock_mcast)
          ; _     <- forkIO (foreverNofail "connectF (databus): " $ connectF db databusEventParser sock_dbus)
          ; _     <- forkIO (start db sock_server)
          ; _     <- installHandler sigINT (Catch $ signal mutex) Nothing
          ; _     <- installHandler sigTERM (Catch $ signal mutex) Nothing
          ; wait mutex
          ; warn "/bye"
          ; exitSuccess
          }
  where setopts _ []               = return ()
        setopts level (Verbose:xs) = setlevel level >> setopts DEBUG xs

        mAttachTo _ Nothing     = return ()
        mAttachTo dbus (Just s) = forkIO (foreverNofail "attachTo: " $ attachTo dbus s) >> return ()
