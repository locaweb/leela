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
import System.Environment
import System.Exit
import System.Posix.Signals
import DarkMatter.Misc
import DarkMatter.Logger
import DarkMatter.Network.Protocol
import DarkMatter.Network.Databus as D
import DarkMatter.Network.Multicast as M
import DarkMatter.Network.TimelineServer

data OptFlag = Verbose
             | Queues Int
             | Threads Int
             deriving (Show)

options :: [OptDescr OptFlag]
options = [ Option "v" ["verbose"] (NoArg Verbose)                     "increase verbosity"
          , Option "q" ["queues"]  (ReqArg (Queues . read) "QUEUES")   "number of queue"
          ]

getopts :: [String] -> ([OptFlag], String, String)
getopts argv = case (getOpt Permute options argv)
               of (o, [p, s], []) -> (o, p, s)
                  (_, _, msg)  -> error (concat msg ++ usageInfo header options)
  where header = "USAGE: wall [-v|--verbose] DATABUS-FILE BROADCAST-FILE"

main :: IO ()
main = do { (opts, pipe, bcast) <- fmap getopts getArgs
          ; setopts INFO opts
          ; mutex <- newEmptyMVar
          ; warn "starting server (^C to terminate)"
          ; dbus  <- newDatabus
          ; group <- newMulticast
          ; _     <- forkIO (foreverNofail "connectF (databus): " $ D.connectF dbus databusMetricParser pipe)
          ; _     <- forkIO (foreverNofail "connectF (multicast): " $ M.connectF group bcast)
          ; start dbus group (getQueues 1 opts)
          ; _     <- installHandler sigINT (Catch $ signal mutex) Nothing
          ; _     <- installHandler sigTERM (Catch $ signal mutex) Nothing
          ; wait mutex
          ; warn "/bye"
          ; exitSuccess
          }
  where setopts _ []               = return ()
        setopts level (Verbose:xs) = setlevel level >> setopts DEBUG xs
        setopts level (_:xs)       = setopts level xs

        getQueues x []             = x
        getQueues _ (Queues x:_)   = x
        getQueues x (_:xs)         = getQueues x xs
