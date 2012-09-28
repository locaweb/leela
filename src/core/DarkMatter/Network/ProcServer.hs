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
-- connections. On each connection, three threads are forked:
-- 
--   1. the `proc' thread, which executes the an arbitrary function
--      over the data;
--
--   2. the `sync' thread, which sends output back to the client;
--
--   3. the `fetch' thread, which reads client's input and feed the
--   proc thread;
module DarkMatter.Network.ProcServer ( start ) where

import           Prelude hiding (null, catch)
import           Control.Concurrent hiding (readChan, writeChan)
import           Control.Concurrent.BoundedChan
import           Control.Exception
import           Data.Function
import           Data.Monoid
import           Blaze.ByteString.Builder (toByteStringIO)
import qualified Data.Foldable as F
import           Network.Socket
import           DarkMatter.Logger (debug, info, warn, crit)
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Parser
import           DarkMatter.Data.Asm.Runtime
import           DarkMatter.Data.Asm.Render
import           DarkMatter.Network.Protocol
import           DarkMatter.Data.Proc

type Input = (Key, Event)

type Output = (Key, Events)

runProc :: BoundedChan (Maybe Input) -> (BoundedChan (Maybe Output)) -> Mode -> [Function] -> IO ()
runProc ichan ochan mode func = evalStateT (modeM mode) (newMultiplex func)
  where putO k e
          | null e    = return ()
          | otherwise = writeChan ochan (Just (k, e))

        modeM Map          = passthrough (readChan ichan) putO
        modeM (Window n m) = window n m (readChan ichan) putO

runFetch :: BoundedChan (Maybe Input) -> IO () -> [Asm] -> IO ()
runFetch chan cont [Event k t v]    = writeChan chan (Just (k, temporal t v)) >> cont
runFetch chan cont (Event k t v:xs) = writeChan chan (Just (k, temporal t v)) >> runFetch chan cont xs
runFetch _ _ (Close:_)              = return ()
runFetch _ _ _                      = crit " error: close|event were expected"

runSync :: Socket -> (BoundedChan (Maybe Output)) -> IO ()
runSync s chan = do { mi <- readChan chan
                    ; case mi
                      of Nothing     -> return ()
                         Just (k, v) -> let key   = renderKey k
                                            build = \acc e -> renderEvent key e <> acc
                                            msg   = F.foldl' build mempty (chk v)
                                        in (toByteStringIO (sendFrame s) msg >> runSync s chan)
                                             `catch` (\(SomeException _) -> crit " error: sending output" >> clearChan)
                    }
  where clearChan = do { mi <- readChan chan
                       ; case mi
                         of Nothing -> return ()
                            _       -> clearChan
                       }


start :: FilePath -> IO ()
start f = do { warn ("binding server on: " ++ f)
             ; s <- socket AF_UNIX Stream 0
             ; bindSocket s (SockAddrUnix f)
             ; listen s 1
             ; fix (\loop -> do { s1 <- fmap fst $ accept s
                                ; info "> connection"
                                ; _ <- forkfinally (go s1) (info "< connection" >> sClose s1)
                                ; loop
                                })
             }
  where go s = do { ochan  <- newBoundedChan 25
                  ; ichan  <- newBoundedChan 5
                  ; asm    <- recvAsm s
                  ; case asm
                    of (Proc m p:xs)
                         -> do { wait <- newEmptyMVar
                               ; debug " forking proc thread"
                               ; _    <- forkfinally (runProc ichan ochan m p) (sendNil ochan)
                               ; debug " exec'ing sync thread"
                               ; _    <- forkfinally (runSync s ochan) (putMVar wait ())
                               ; (if (null xs)
                                  then fix (\loop -> recvAsm s >>= runFetch ichan loop)
                                  else runFetch ichan (fix (\loop -> recvAsm s >>= runFetch ichan loop)) xs)
                                   `finally` (sendNil ichan)
                               ; takeMVar wait
                               }
                       _
                         -> crit " error: single proc was expected"
                  }
        
recvAsm :: Socket -> IO [Asm]
recvAsm h = fmap runAll (recvFrame h)

sendNil :: BoundedChan (Maybe a) -> IO ()
sendNil = flip writeChan Nothing

forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (show e) >> after
        f _                        = after

