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

import Control.Concurrent.MVar
import Control.Concurrent
import Network.Socket (SockAddr(..))
import System.Environment
import System.Exit
import System.Posix.Signals
import DarkMatter.Misc
import DarkMatter.Logger
import DarkMatter.Network.Multicast
import DarkMatter.Network.MulticastServer

data OptFlag = Verbose
             deriving (Show)

getopts :: [String] -> Either String (String, String, Maybe String)
getopts [x, y]    = Right (x, y, Nothing)
getopts [x, y, z] = Right (x, y, Just z)
getopts _         = Left "USAGE: multicast DATABUS-FILE ANYCAST-FILE [PARENT-BROADCAST]"

main :: IO ()
main = fmap getopts getArgs >>= dispatch
  where dispatch (Left msg)
          = putStrLn msg
        dispatch (Right (sfile, mfile, pfile)) =
          do { mutex <- newEmptyMVar
             ; warn "starting server (^C to terminate)"
             ; m     <- newMulticast Anycast
             ; _     <- forkIO (foreverNofail "multicast: " $ connectF m mfile)
             ; _     <- forkIO (foreverNofail "server: " $ start m sfile)
             ; _     <- forkIO (foreverNofail "attachTo: " mAttachTo)
             ; _     <- installHandler sigINT (Catch $ signal mutex) Nothing
             ; _     <- installHandler sigTERM (Catch $ signal mutex) Nothing
             ; wait mutex
             ; warn "/bye"
             ; exitSuccess
             }
            where mAttachTo = case pfile
                              of Nothing -> return ()
                                 Just f  -> attachTo sfile (SockAddrUnix f)



