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

-- | This modules reads from a file handle allows threads to be
-- attached to read items out of the bus.
module DarkMatter.Network.Databus
       ( Databus()
       , Wire()
       , Chunk(..)
       -- * Constructors
       , newDatabus
       -- * Databus operations
       , connectF
       , connectS
       , attach
       , detach
       -- * Wire operations
       , term
       , wireRead
       , wireWrite
       ) where

import           Control.Monad
import           Control.Exception
import           Control.Concurrent.STM
import           System.Directory
import qualified Data.ByteString as B
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString
import           DarkMatter.Data.Proc

type Databus a = TVar (DatabusT a)

-- | The event bus.
data DatabusT a = Databus { connections :: [Wire a]
                          , wsequence   :: Int
                          }

data State = Read
           | ReadWrite
           deriving (Eq)

-- | This represents a connection to the databus.
data Wire a = Wire { uid    :: Int
                   , queue  :: TBQueue (Chunk a)
                   , select :: a -> Maybe a
                   , state  :: TVar State
                   }

data Chunk a = Chunk a
             | Empty
             | EOF
             deriving (Eq)

-- | Creates a new empty databus.
newDatabus :: IO (Databus a)
newDatabus = newTVarIO (Databus [] 0)

ifStateOrElse :: Wire a -> (State -> Bool) -> STM b -> STM b -> STM b
ifStateOrElse w f a b = do { s <- readTVar (state w)
                           ; if (f s)
                             then a
                             else b
                           }

-- | Read an item from the wire. May block if the queue is empty. If
-- the wire has been closed and the queue is empty, this function
-- returns EOF.
wireRead :: Wire a -> IO (Chunk a)
wireRead w = atomically $ ifStateOrElse w (== ReadWrite) doRead checkRead
  where doRead    = readTBQueue $ queue w
        checkRead = do { mv <- tryReadTBQueue (queue w)
                       ; case (mv)
                         of Nothing -> return $ EOF
                            Just x  -> return x
                       }

-- | Write something into the wire. May block if the queue is full. If
-- the wire has been closed, this function does nothing.
wireWrite :: Wire a -> a -> IO ()
wireWrite w a = atomically $ ifStateOrElse w (== ReadWrite) doWrite (return ())
  where doWrite = writeTBQueue (queue w) (Chunk a)

-- | Shutdown any writes into this wire. Remember you still must
-- invoke detach.
term :: Wire a -> IO ()
term = atomically . termT

termT :: Wire a -> STM ()
termT w = writeTVar (state w) Read

-- | Version of connect that works with a FilePath.
connectF :: Databus a -> Proc B.ByteString a -> FilePath -> IO ()
connectF db p f = bracket cOpen cClose (connectS db p)
  where cOpen = do { s <- socket AF_UNIX Datagram 0
                   ; mapM_ (uncurry (setSocketOption s)) [(SendBuffer, 65536)]
                   ; bindSocket s (SockAddrUnix f)
                   ; return s
                   }

        cClose s = sClose s >> removeFile f

-- | This effectively starts the databus. This function should block
-- indefinitely, at least if no errors nor an EOF happens.
--
-- Each item that is produced by the parser is broadcasted over
-- current attached wires. Notice that if the queue is full, this
-- function blocks.
connectS :: Databus a -> Proc B.ByteString a -> Socket -> IO ()
connectS db parse fh = fetch parse
  where fetch p = do { (a, p1) <- fmap (run1 p) (recv fh 65536)
                     ; broadcast a
                     ; fetch p1
                     }

        broadcast a = fmap connections (readTVarIO db) >>= mapM_ doWrite
          where doWrite w = case (select w a)
                            of Nothing -> return ()
                               Just b  -> wireWrite w b

-- | Attachs a new listener to the bus. This function returns queue
-- that will get all the contents of the input bus.
--
-- It is extremely important to continuously consume the wire
-- (@wireRead@) and detach when done. Failing to do so will block the
-- databus and all other threads.
attach :: Databus a -> (a -> Maybe a) -> IO (Wire a)
attach db f = atomically $ doAttach
  where doAttach = do { wuid        <- nextid db
                      ; wire        <- liftM2 (\q s -> Wire wuid q f s) (newTBQueue 5) (newTVar ReadWrite)
                      ; modifyTVar db (flip attachWire wire)
                      ; return wire
                      }

-- | Destroy an wire previously created using attach.
detach :: Databus a -> Wire a -> IO ()
detach db w = atomically (termT w >> drain >> destroy)
  where drain = do { mv <- tryReadTBQueue (queue w)
                   ; case mv
                     of Nothing -> writeTBQueue (queue w) Empty
                        Just _  -> drain
                   }

        destroy = modifyTVar db (flip removeWire w)

removeWire :: DatabusT a -> Wire a -> DatabusT a
removeWire db w0 = db { connections = remove (connections db) }
  where remove []            = []
        remove (w:ws)
          | uid w0 == uid w = ws
          | otherwise       = w : remove ws

attachWire :: DatabusT a -> Wire a -> DatabusT a
attachWire db wire = db { connections = wire : (connections db) }

nextid :: Databus a -> STM Int
nextid tdb = do { db <- readTVar tdb
                ; writeTVar tdb (db { wsequence = (wsequence db + 1) })
                ; return (wsequence db)
                }
