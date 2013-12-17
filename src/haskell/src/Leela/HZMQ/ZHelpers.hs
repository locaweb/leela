-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
--    
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--    
--     http://www.apache.org/licenses/LICENSE-2.0
--    
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Leela.HZMQ.ZHelpers where

import Data.Int
import System.ZMQ3
import Data.ByteString (ByteString)
import Leela.Data.Endpoint

recvTimeout :: (Receiver a) => Int64 -> Socket a -> IO (Maybe [ByteString])
recvTimeout t s = do
  ready <- poll t [Sock s [In] Nothing]
  case ready of
    [[]] -> return Nothing
    _    -> fmap Just (receiveMulti s)

ms :: Int -> Int
ms = (* 1000)

configure :: Socket a -> IO ()
configure fh = do
  setLinger (restrict (ms 0)) fh
  setReconnectInterval (restrict (ms 1)) fh
  setMaxMessageSize (restrict (1024*1024 :: Int)) fh

toEndpoint :: Endpoint -> [String]
toEndpoint e
  | isTCP e   = map buildItem (eAddr e)
  | otherwise = ["tcp://localhost:4080"]
    where
      buildItem (h, Nothing) = "tcp://" ++ h ++ ":50021"
      buildItem (h, Just p)  = "tcp://" ++ h ++ ":" ++ show p

toEndpoint1 :: Endpoint -> String
toEndpoint1 = head . toEndpoint
