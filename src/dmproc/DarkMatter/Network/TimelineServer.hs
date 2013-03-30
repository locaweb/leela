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

import           Data.Bits
import           Control.Concurrent
import           Control.Exception
import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Hashable (hash)
import           DarkMatter.Logger (info, crit)
import           DarkMatter.Data.Metric
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

drainWire :: Timeline Key -> Wire Input -> Multicast -> IO ()
drainWire w input group = do { mc <- wireRead input
                             ; case (mc)
                               of EOF      -> return ()
                                  Empty    -> drainWire w input group
                                  Chunk [] -> drainWire w input group
                                  Chunk c  -> let (w1, events) = publishMany w c
                                              in broadcast events >> drainWire w1 input group
                              }
  where broadcast [] = return ()
        broadcast es = let doc = renderList (\(k, e) -> renderEvent (fromByteString k) e) es
                       in multicast group (toByteString doc)

start :: Databus Input -> Multicast -> Int -> IO ()
start input group queues = do { info $ "starting " ++ show queues ++ " timeline queues"
                              ; forks <- mapM (flip fire empty) [0..(size-1)]
                              ; info $ "timeline working!"
                              ; mapM_ wait forks
                              }
  where select myid = pack . filter (\x -> (hash x .&. (size-1)) == myid)
            where pack [] = Nothing
                  pack xs = Just xs

        fire myid wall = do { info $ "creating timeline queue (" ++ show myid ++ ")"
                            ; mutex <- newEmptyMVar
                            ; wire  <- attach input (select myid)
                            ; _     <- forkfinally (drainWire wall wire group) (signal mutex)
                            ; return mutex
                            }

        size = 2 ^ queues
  
forkfinally :: IO () -> IO () -> IO ThreadId
forkfinally action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (show e) >> after
        f _                        = after

