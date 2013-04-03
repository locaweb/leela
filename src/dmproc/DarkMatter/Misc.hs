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

module DarkMatter.Misc where

import Control.Monad
import Control.Concurrent
import Control.Exception
import DarkMatter.Logger

wait :: MVar () -> IO ()
wait = takeMVar

signal :: MVar () -> IO ()
signal = flip putMVar ()

forkfinally :: String -> IO () -> IO () -> IO ThreadId
forkfinally msg action after =
    mask $ \restore ->
      forkIO $ try (restore action) >>= f
  where f (Left (SomeException e)) = crit (msg ++ show e) >> after
        f _                        = after

foreverNofail :: String -> IO () -> IO ()
foreverNofail msg io = forever $ try io >>= f
  where f (Left (SomeException e)) = crit (msg ++ show e) >> delay
        f _                        = delay

        delay = threadDelay (1 * 1000000)
