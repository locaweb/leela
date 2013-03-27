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

-- | This modules exposes some sort of broadcast service through unix
-- sockets. Everyone that wants to hear from it must send a message
-- periodically (ideally every sec). The peer addr is used to broadcat
-- the message.
module DarkMatter.Network.Databus
       ( Broadcast()
       , Peer(..)
       , newBroadcast
       , addPeer
       , delPeer
       , withPeers
       ) where

import Data.Function
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import System.Mem.Weak (addFinalizer)
import Network.Socket hiding (Broadcast)

data Peer k v = Peer { uid    :: k
                     , val    :: v
                     , ttl    :: Int
                     , kill   :: IO ()
                     }

newtype Broadcast k v = Broadcast { peers :: TVar [Peer k v] }

-- | Creates a new empty broadcast. You may manage the peer list using
-- addPeer and delPeer functions.
newBroadcast :: IO (Broadcast k v)
newBroadcast = do { broadcast <- fmap Broadcast (newTVarIO [])
                  ; reaperId  <- forkIO $ reaper broadcast
                  ; addFinalizer broadcast (killThread reaperId)
                  ; return broadcast
                  }

addPeerT :: (Eq k) => Peer k v -> Broadcast k v -> STM Bool
addPeerT p1 b = do { ring <- readTVar (peers b)
                   ; if (p1 `notElem` ring)
                     then writeTVar (peers b) (updateTTL ring) >> return False
                     else writeTVar (peers b) (p1 : ring) >> return True
                   }
  where updateTTL []  = []
        updateTTL (p:ps)
          | p == p1   = p { ttl = ttl p1 } : ps
          | otherwise = p : updateTTL ps

-- | Adds a new to the list. If this is already on the broadcast list
-- the kill method is invoked.
addPeer :: (Eq k) => Peer k v -> Broadcast k v -> IO ()
addPeer p b = do { success <- atomically $ addPeerT p b
                 ; if (not success)
                   then kill p
                   else return ()
                 }

delPeerT :: (Eq k) => k -> Broadcast k v -> STM (Maybe (Peer k v))
delPeerT k b = do { (mp, ring) <- fmap (extract []) (readTVar (peers b))
                  ; writeTVar (peers b) ring
                  ; return mp
                  }
  where extract acc [] = (Nothing, acc)
        extract acc (p:ps)
          | uid p == k = (Just p, acc ++ ps)
          | otherwise  = extract (p:acc) ps

-- | Remove a peer from the broadcast list and invokes its kill
-- function.
delPeer :: (Eq k) => k -> Broadcast k v -> IO ()
delPeer k b = do { mp <- atomically $ delPeerT k b
                 ; case (mp)
                   of Nothing -> return ()
                      Just p  -> kill p
                 }

-- | Applies a function for every peer in the broadcast list. If this
-- function throws an error, this peer gets removed from the list
-- using delPeer method.
withPeers :: (Eq k) => Broadcast k v => (Peer k v -> IO ()) -> IO ()
withPeers b exe = atomically (readTVar (peers b)) >>= mapM_ runIO
  where runIO peer = exe peer `onException` delPeer (uid peer) b

-- | The thread responsible for cleaning up stale resources. This
-- function run forever (it blocks).
reaper :: Broadcast k v -> IO ()
reaper b = forever $ do { threadDelay (1 * 1000000)
                        ; atomically (gc b) >>= mapM_ kill
                        }

-- | The gc function. It first decreases the TTL of every peer in the
-- list then it removes the ones with ttl <= 0.
gc :: Broadcast k v -> STM [Peer k v]
gc b = do { (l, r) <- fmap (expire ([], []) . map decrTTL) (readTVar (peers b))
          ; writeTVar (peers b) r
          ; return l
          }
  where decrTTL p = p { ttl = (ttl p) - 1 }

        expire lr []  = lr
        expire (l, r) (x:xs)
          | ttl x > 0 = expire (l, x : r) xs
          | otherwise = expire (x : l, r) xs

instance Eq k => Eq (Peer k v) where
  (==) = (==) `on` uid

instance Ord k => Ord (Peer k v) where
  compare = compare `on` uid
