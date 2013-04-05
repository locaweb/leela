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
module DarkMatter.Network.MulticastServer ( start ) where

import           Control.Monad
import           Control.Exception (bracket)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString
import           System.Directory
import           DarkMatter.Network.Multicast

work :: Multicast -> Socket -> IO ()
work mcast server = forever $ recv server maxpacket >>= multicast mcast

start :: Multicast -> FilePath -> IO ()
start mcast server = bracket cOpen cClose (work mcast)
  where cOpen = do { s <- socket AF_UNIX Datagram 0
                   ; mapM_ (uncurry (setSocketOption s)) [(RecvBuffer, maxpacket),
                                                          (SendBuffer, maxpacket)
                                                         ]
                   ; bindSocket s (SockAddrUnix server)
                   ; return s
                   }

        cClose s = close s >> removeFile server
