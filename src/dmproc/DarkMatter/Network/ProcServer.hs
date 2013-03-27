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

-- | This module creates a new unix socket to listen for
-- connections. The main thread accepts the requests and one new
-- connection is made another one is created to handle the request.
module DarkMatter.Network.ProcServer ( start ) where

import Prelude
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.Function
import Blaze.ByteString.Builder
import Network.Socket
import DarkMatter.Logger (debug, info, warn, crit)
import DarkMatter.Data.Event
import DarkMatter.Data.Asm.Types hiding (Stream)
import DarkMatter.Data.Asm.Runtime
import DarkMatter.Data.Parsers.Helpers
import DarkMatter.Data.Parsers.AsmParser
import DarkMatter.Data.Parsers.AsmPP
import DarkMatter.Network.Protocol
import DarkMatter.Network.Databus

type Input = [(Key, Event)]

type Output = (Key, Event)

wait :: MVar () -> IO ()
wait = takeMVar

signal :: MVar () -> IO ()
signal = flip putMVar ()

runProc :: IO (Chunk Input) -> (Output -> IO ()) -> [Function] -> IO ()
runProc getI0 putO func = evalStateT (exec getI putO) (newMultiplex func)
  where getI = do { mv <- getI0
                  ; case mv
                    of EOF     -> return Nothing
                       Empty   -> getI
                       Chunk c -> return $ Just c
                  }

handleRequest :: Socket -> Databus Input -> Mode -> [Function] -> IO ()
handleRequest s bus (Match m) p =
  do { sendStatus s Success
     ; sem  <- newEmptyMVar
     ; wire <- attach bus (databusMatcher (snd m))
     ; debug (" forking proc thread: " ++ toString (render $ Proc (Match m) p))
     ; _    <- forkfinally (runProc (wireRead wire) (sockWrite s) p)
                           (detach bus wire >> signal sem)
     ; asm <- recvAsm s
     ; when (asm /= [Close]) (warn "error: `close;' was expected")
     ; term wire
     ; wait sem
     }
handleRequest s _ _ _ = sendStatus s Failure

sockWrite :: Socket -> Output -> IO ()
sockWrite s (k, e) = let msg = renderEvent (renderStr k) e
                     in toByteStringIO (sendFrame s) msg

start :: Databus Input -> FilePath -> IO ()
start bus f = do { warn ("binding server on: " ++ f)
                 ; s <- socket AF_UNIX Stream 0
                 ; bindSocket s (SockAddrUnix f)
                 ; listen s 1
                 ; fix (\loop -> do { s1 <- fmap fst $ accept s
                                    ; info "creating new connection"
                                    ; _ <- forkfinally (go s1) (info "droppping connection" >> sClose s1)
                                    ; loop
                                    })
                 }
  where go s = do { asm <- recvAsm s
                  ; case asm
                    of [Proc m p]
                         -> handleRequest s bus m p
                       _
                         -> do { sendStatus s Failure
                               ; crit " error: single proc was expected as the fst instruction"
                               }
                  }

recvAsm :: Socket -> IO [Asm]
recvAsm h = fmap (runAll asmParser) (recvFrame h)

forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (show e) >> after
        f _                        = after

