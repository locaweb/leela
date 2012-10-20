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

-- | We are using unix sockets + SEQPACKET so there is no need to
-- defined a network protocol. The only thing this does is define the
-- buffer size.
module DarkMatter.Network.Protocol where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Network.Socket (Socket)
import           Network.Socket.ByteString
import           DarkMatter.Data.Asm.Types (Key)
import           DarkMatter.Data.Asm.Parser
import           DarkMatter.Data.Event
import           DarkMatter.Data.Proc

data Status = Success
            | Failure

unpackShort :: B.ByteString -> Int
unpackShort n = let [a, b] = map fromIntegral (B.unpack n)
                in (a `shiftL` 8) .|. b

packShort :: Int -> B.ByteString
packShort i0 = let a = fromIntegral (i0 `shiftR` 8 .&. 0xFF)
                   b = fromIntegral (i0 .&. 0xFF)
               in B.pack [a, b]

recvFrame :: Socket -> IO B.ByteString
recvFrame s = do { sz <- recv s 2
                 ; if (B.length sz == 2)
                   then recv s (unpackShort sz)
                   else return B.empty
                 }

sendFrame :: Socket -> B.ByteString -> IO ()
sendFrame s msg = let sz = B.length msg
                  in sendAll s (packShort sz `B.append` msg)

sendStatus :: Socket -> Status -> IO ()
sendStatus s Success = sendFrame s (B8.pack "status 0;")
sendStatus s Failure = sendFrame s (B8.pack "status 1;")

eol :: B.ByteString
eol = B8.singleton ';'

databusParser :: Proc B.ByteString [(Key, Event)]
databusParser = Auto $ f B.empty
  where f l r = let (frame, leftover) = B8.breakEnd (== ';') (B.append l r)
                    events            = runAll eventParser frame
                in (events , Auto $ f leftover)

