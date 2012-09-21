{-# LANGUAGE OverloadedStrings #-}
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

module DarkMatter.Network.CoreServer where

import           Control.Concurrent hiding (readChan, writeChan)
import           Control.Concurrent.BoundedChan
import           Control.Exception
import           Control.Monad.Trans.State
import           Control.Monad.Trans
import           Data.Function
import qualified Data.Map as M
import           Network.Socket
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Asm.Parser
import           DarkMatter.Data.Asm.Runtime
import           DarkMatter.Network.Protocol

type Input = (Key, Event)

type Output = (Key, Events)

type State = M.Map Key Pipeline

recvAsm :: Socket -> IO [Asm]
recvAsm h = fmap runAll (recvFrame h)

creat :: (BoundedChan (Maybe Input)) -> (BoundedChan (Maybe Output)) -> StateT (Pipeline, M.Map Key Pipeline) IO ()
creat ichan ochan = do { mi <- liftIO (readChan ichan)
                       ; case mi
                         of Just (k, e) -> do { multiplex k e >>= liftIO . sendData ochan k
                                              ; creat ichan ochan
                                              }
                            Nothing     -> liftIO $ sendEOF ochan
                       }

sendData :: BoundedChan (Maybe (k, v)) -> k -> v -> IO ()
sendData chan k v = writeChan chan (Just (k, v))

sendEOF :: BoundedChan (Maybe a) -> IO ()
sendEOF = flip writeChan Nothing

forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f :: Either SomeException a -> IO ()
        f = const after

stream :: Socket -> BoundedChan (Maybe Input) -> BoundedChan (Maybe Output) -> IO ()
stream h ichan ochan = do
    { wait <- newEmptyMVar
    ; prog <- recvAsm h
    ; case prog
      of (Proc p:xs)
           -> do { _ <- forkIO (xThread p >> putMVar wait ())
                 ; if (null xs)
                   then fix (\loop -> (recvAsm h >>= go loop))
                   else go (fix (\loop -> (recvAsm h >>= go loop))) xs
                 ; takeMVar wait
                 }
         _
           -> return ()
    }
  where xThread = evalStateT (creat ichan ochan) . newMultiplex

        go cont [Event k t v]    = sendData ichan k (temporal t v) >> cont
        go cont (Event k t v:xs) = sendData ichan k (temporal t v) >> go cont xs
        go _ _                   = sendEOF ichan
    
start :: FilePath -> IO ()
start f = do { s <- socket AF_UNIX SeqPacket 0
             ; bindSocket s (SockAddrUnix f)
             ; listen s 5
             ; fix (\loop -> do { c <- fmap fst (accept s)
                                ; _ <- forkfinally (go c) (sClose c)
                                ; loop
                                })
             }
  where go c = do { ochan  <- newBoundedChan 5
                  ; ichan  <- newBoundedChan 10
                  ; _      <- forkIO (stream c ichan ochan)
                  ; fix (\loop -> do { mi <- readChan ochan
                                     ; case mi
                                       of Nothing -> return ()
                                          Just _  -> loop
                                     })
                  }

