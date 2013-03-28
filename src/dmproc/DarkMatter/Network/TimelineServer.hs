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
module DarkMatter.Network.TimelineServer ( start ) where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Hashable
import           Data.Maybe
import           DarkMatter.Logger (info, crit)
import           DarkMatter.Data.Metric
import           DarkMatter.Data.Event
import           DarkMatter.Data.Timeline
import           DarkMatter.Data.Parsers.Helpers
import           DarkMatter.Network.Databus
import           DarkMatter.Network.Multicast

type Key = B.ByteString

type Input = [Metric Key]

wait :: MVar () -> IO ()
wait = takeMVar

signal :: MVar () -> IO ()
signal = flip putMVar ()

drainWire :: TVar (Timeline Key) -> Wire Input -> Multicast -> IO ()
drainWire tw input group = do { mc <- wireRead input
                              ; case (mc)
                                of EOF     -> return ()
                                   Empty   -> drainWire tw input group
                                   Chunk c -> do { events <- fmap catMaybes (mapM (updateTimeline tw) c)
                                                 ; when (length events > 0) (broadcast events)
                                                 ; drainWire tw input group
                                                 }
                              }
  where broadcast events = let msg = renderList (\(k, e) -> renderEvent (fromByteString k) e) events
                           in multicast group (toByteString msg)

updateTimeline :: TVar (Timeline Key) -> Metric Key -> IO (Maybe (Key, Event))
updateTimeline tw m = atomically $ do { (w, me) <- fmap (flip publish m) (readTVar tw)
                                      ; writeTVar tw w
                                      ; return me
                                      }

start :: Databus Input -> Multicast -> Int -> Int -> IO ()
start input group queues tpq = do { info $ "starting " ++ show queues ++ " timeline queues"
                                  ; forks <- mapM (\myid -> newTVarIO empty >>= fire myid) [0..(queues-1)]
                                  ; _     <- mapM_ (mapM_ wait) forks
                                  ; info $ "timeline working!"
                                  }
  where select myid = filter ((== myid) . (`mod` queues) . hash)

        fire myid wall = do { info $ "creating threads for queue (" ++ show myid ++ "/" ++ show tpq ++ ")"
                            ; mutexes <- replicateM tpq newEmptyMVar
                            ; wire    <- attach input (Just . select myid)
                            ; mapM_ (forkfinally (drainWire wall wire group) . signal) mutexes
                            ; return mutexes
                            }
  
forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (show e) >> after
        f _                        = after

