{-# LANGUAGE ScopedTypeVariables #-}
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

-- | This modules exposes some sort of broadcast service through unix
-- sockets. Everyone that wants to hear from it must send a `attach`
-- message periodically (ideally every sec). The peer address is the
-- one registered in the broadcast group.
module DarkMatter.Network.Multicast
       ( Multicast()
       , connectF
       , connectS
       , newMulticast
       , addPeer
       , delPeer
       , multicast
       ) where

import qualified Data.ByteString as B
import           System.Directory
import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.HashMap as M
import           System.Mem.Weak (addFinalizer)
import           Network.Socket hiding (sendTo)
import           Network.Socket.ByteString (sendTo)
import           DarkMatter.Logger (info)

data Multicast = Multicast { peers   :: TVar (M.Map FilePath Int)
                           , channel :: Socket
                           }

maxpacket :: Int
maxpacket = 65536

connectF :: Multicast -> FilePath -> IO ()
connectF g f = bracket cOpen cClose (connectS g)
  where cOpen = do { s <- socket AF_UNIX Datagram 0
                   ; mapM_ (uncurry (setSocketOption s)) [(RecvBuffer, maxpacket),
                                                          (SendBuffer, maxpacket)
                                                         ]
                   ; bind s (SockAddrUnix f)
                   ; return s
                   }

        cClose s = sClose s >> removeFile f

connectS :: Multicast -> Socket -> IO ()
connectS g fh = forever $  do { (msg, _, peerAddr) <- recvFrom fh maxpacket
                              ; case (peerAddr)
                                of SockAddrUnix peer
                                     | msg == "attach\n" -> do { info ("attaching new peer: " ++ peer)
                                                               ; addPeer g peer
                                                               }
                                     | otherwise         -> return ()
                                   _                     -> return ()
                              }

newMulticast :: IO Multicast
newMulticast = do { s <- socket AF_UNIX Datagram 0
                  ; g <- newTVarIO M.empty
                  ; let group = Multicast g s
                  ; mapM_ (uncurry (setSocketOption s)) [(RecvBuffer, maxpacket),
                                                         (SendBuffer, maxpacket)
                                                        ]
                  ; reaperId <- forkIO $ reaper group
                  ; addFinalizer group (killThread reaperId)
                  ; return group
                  }

addPeer :: Multicast -> FilePath -> IO ()
addPeer g k = atomically $ modifyTVar (peers g) (M.insert k 5)

delPeer :: Multicast -> FilePath -> IO ()
delPeer g k = atomically $ modifyTVar (peers g) (M.delete k)

multicast :: Multicast -> B.ByteString -> IO ()
multicast g msg = atomically (readTVar (peers g)) >>= mapM_ runIO . M.keys
  where runIO peer = send_ (SockAddrUnix peer) `catch` (\(_ :: SomeException) -> delPeer g peer)
        send_ peer = sendTo (channel g) msg peer >> return ()

reaper :: Multicast -> IO ()
reaper m = forever $ do { threadDelay (1 * 1000000)
                        ; atomically (gc m)
                        }

gc :: Multicast -> STM ()
gc g = modifyTVar (peers g) (M.foldWithKey expire M.empty)
  where expire k v m
          | v <= 0    = m
          | otherwise = M.insert k (v-1) m
