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

import Control.Exception
import System.Console.GetOpt
import Control.Concurrent.MVar
import Control.Concurrent
import System.Environment
import System.Directory
import System.Exit
import System.Posix.Signals
import DarkMatter.Logger
import DarkMatter.Network.ProcServer

data OptFlag = Verbose
             | Version
             deriving (Show)

options :: [OptDescr OptFlag]
options = [ Option "v" ["verbose"] (NoArg Verbose) "increase verbosity"
          , Option ""  ["version"] (NoArg Version) "show version and exit"
          ]

getopts :: [String] -> ([OptFlag], String)
getopts argv = case (getOpt Permute options argv)
               of (o, [s], []) -> (o, s)
                  (_, _, msg)  -> error (concat msg ++ usageInfo header options)
  where header = "USAGE: dmproc [-v|--verbose] [--version] SOCKET-FILE"

main :: IO ()
main = do { (opts, socket) <- fmap getopts getArgs
          ; setopts INFO opts
          ; wait <- newEmptyMVar
          ; warn "starting server (^C to terminate)"
          ; _    <- forkIO (start socket)
          ; _    <- installHandler sigINT (Catch $ putMVar wait ()) Nothing
          ; _    <- installHandler sigTERM (Catch $ putMVar wait ()) Nothing
          ; takeMVar wait
          ; warn "/bye"
          ; removeFile socket
          ; exitSuccess
          }
  where setopts _ []               = return ()
        setopts level (Verbose:xs) = setlevel level >> setopts DEBUG xs
        setopts _ (Version:_)      = error "todo:fixme"
