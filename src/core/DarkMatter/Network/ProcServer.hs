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

module DarkMatter.Network.ProcServer where

import           Prelude hiding (null)
import           Control.Concurrent hiding (readChan, writeChan)
import           Control.Concurrent.BoundedChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Function
import           Data.Monoid
import           Blaze.ByteString.Builder (toByteStringIO)
import qualified Data.Foldable as F
import qualified Data.Map as M
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

type State = M.Map Key Pipeline

recvAsm :: Socket -> IO [Asm]
recvAsm h = fmap runAll (recvFrame h)

runProc :: (BoundedChan (Maybe Input)) -> (BoundedChan (Maybe Output)) -> Mode -> [Function] -> IO ()
runProc ichan ochan mode func = evalStateT (modeM mode) (newMultiplex func)
  where close = do { debug "closing worker runProc thread"
                   ; liftIO (sendNil ochan)
                   }

        putO k e = when (not (null e)) (writeChan ochan $ Just (k, e))

        modeM ForEach      = forEach (readChan ichan) putO close
        modeM (Window _ _) = error "todo:fixme"

sendData :: BoundedChan (Maybe (k, v)) -> k -> v -> IO ()
sendData chan k v = writeChan chan (Just (k, v))

sendNil :: BoundedChan (Maybe a) -> IO ()
sendNil = flip writeChan Nothing

forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (show e) >> after
        f _                        = after

stream :: Socket -> BoundedChan (Maybe Input) -> BoundedChan (Maybe Output) -> IO ()
stream h ichan ochan = do
    { wait <- newEmptyMVar
    ; prog <- recvAsm h
    ; case prog
      of (Proc m p:xs)
           -> do { debug " forking worker thread"
                 ; _ <- forkIO (runProc ichan ochan m p >> putMVar wait ())
                 ; if (null xs)
                   then fix (\loop -> (recvAsm h >>= go loop))
                   else go (fix (\loop -> (recvAsm h >>= go loop))) xs
                 ; debug " worker thread exiting"
                 ; takeMVar wait
                 }
         _
           -> do { warn " error: invalid instruction (proc was expected)"
                 ; sendNil ochan
                 }
    }
  where go cont [Event k t v]    = sendData ichan k (temporal t v) >> cont
        go cont (Event k t v:xs) = sendData ichan k (temporal t v) >> go cont xs
        go _ (Close:_)           = sendNil ichan
        go _ _                   = do { warn " error: invalid instruction (event was excpected)"
                                      ; sendNil ichan
                                      }
    
start :: FilePath -> IO ()
start f = do { warn ("binding server on: " ++ f)
             ; s <- socket AF_UNIX SeqPacket 0
             ; bindSocket s (SockAddrUnix f)
             ; listen s 5
             ; fix (\loop -> do { s1 <- fmap fst $ accept s
                                ; info "> connection"
                                ; _ <- forkfinally (go s1) (info "< connection" >> sClose s1)
                                ; loop
                                })
             }
  where go c = do { ochan  <- newBoundedChan 2
                  ; ichan  <- newBoundedChan 75
                  ; debug " forking flushing thread"
                  ; _      <- forkIO (stream c ichan ochan)
                  ; fix (\loop -> do { mi <- readChan ochan
                                     ; case mi
                                       of Nothing     -> return ()
                                          Just (k, v) -> let key   = renderKey k
                                                             build = \acc e -> renderEvent key e <> acc
                                                             msg   = F.foldl' build mempty (chk v)
                                                         in toByteStringIO (sendFrame c) msg >> loop
                                     })
                  ; debug " flusing thread exiting"
                  }
