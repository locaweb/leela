{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
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

module DarkMatter.Data.Asm.Runtime
       ( Events
       , Pipeline
       , Runtime
       , eofChunk
       , pipeline
       , newMultiplex
       , multiplex1
       , multiplex
       , broadcastEOF
       , proc
       , forEach
       , evalStateT
       ) where

import           Prelude hiding (null)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Function (on)
import           Data.Monoid (mempty)
import qualified Data.Map as M
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

type Events = ChunkC [Event]

type Pipeline = Proc Events Events

type Runtime m a = StateT (Pipeline, M.Map Key Pipeline) m a

mproc :: (Event -> Event) -> Proc Events Events
mproc f = pureF g
  where g c = fmap (map f) c

fproc :: (Event -> a) -> (a -> a -> a) -> (a -> [Event]) -> Proc Events Events
fproc f g h = await (go Nothing)
  where go macc c
          | eof c     = done (new (h tmp) True)
          | null c    = await (go macc)
          | otherwise = await (go (Just tmp))
            where tmp = case macc
                        of Nothing       -> foldr1 g (map f (chk c))
                           Just acc
                             | null c    -> acc
                             | otherwise -> acc `seq` (acc `g` foldr1 g (map f (chk c)))

decomp :: Event -> (Time, Double)
decomp e = (time e, val e)

compose :: (a -> a -> a) -> (b -> b -> b) -> (a, b) -> (a, b) -> (a, b)
compose f g (t0, v0) (t1, v1) = (f t0 t1, g v0 v1)

build :: (Time, Double) -> [Event]
build (t, d) = [temporal t d]

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp a b
  | cmp a b == LT = b
  | otherwise     = a

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp a b
  | cmp a b == GT = b
  | otherwise     = a

proc :: Function -> Pipeline
proc Sum                = fproc (decomp)
                                (compose (min) (+))
                                (build)
proc Prod               = fproc (decomp)
                                (compose (min) (*))
                                (build)
proc Mean               = fproc (\e -> (decomp e, 1))
                                (\(v0, n0) (v1, n1) -> (compose min (+) v0 v1, n0+n1))
                                (\((t, v), n) -> [temporal t (v/n)])
proc Median             = error "todo: fixme"
proc Abs                = mproc (update id abs)
proc Ceil               = mproc (update id (fromInteger . ceiling))
proc Floor              = mproc (update id (fromInteger . floor))
proc Round              = mproc (update id (fromInteger . round))
proc Truncate           = mproc (update id (fromInteger . truncate))
proc Maximum            = fproc id (maxBy (compare `on` val)) (:[])
proc Minimum            = fproc id (minBy (compare `on` val)) (:[])
proc (Arithmetic Div t) = mproc (update id (/ t))
proc (Arithmetic Sub t) = mproc (update id (flip (-) t))
proc (Arithmetic Mul t) = mproc (update id (* t))
proc (Arithmetic Add t) = mproc (update id (+ t))

pipeline :: [Function] -> Pipeline
pipeline = foldr1 pipe . map proc

getenv :: (Monad m) => Key -> Runtime m (Pipeline, Pipeline)
getenv k = do { (z, m) <- get
              ; case M.lookup k m
                of Nothing -> return (z, z)
                   Just y  -> return (z, y)
              }

putenv :: (Monad m) => Key -> Pipeline -> Runtime m ()
putenv k p = do { (z, m) <- get
                ; put (z, M.insert k p m)
                }

eofChunk :: Events
eofChunk = new mempty True

forEach :: (IO (Maybe (Key, Event))) -> (Key -> Events -> IO ()) -> IO () -> Runtime IO ()
forEach getI putO close = do { mi <- liftIO getI
                             ; case mi
                               of Nothing    -> do { broadcastEOF >>= liftIO . mapM_ (uncurry putO)
                                                   ; liftIO close
                                                   }
                                  Just (k,e) -> do { multiplex1 k e >>= liftIO . putO k
                                                   ; forEach getI putO close
                                                   }
                             }

newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

multiplex1 :: (Monad m) => Key -> Event -> Runtime m Events
multiplex1 k = multiplex k . new_ . (:[])

multiplex :: (Monad m) => Key -> Events -> Runtime m Events
multiplex k i = do { (z, p) <- getenv k
                   ; case (eval $ feed p i)
                     of Left f        
                          -> putenv k f >> return mempty
                        Right o
                          -> putenv k z >> return o
                   }

broadcastEOF :: (Monad m, Functor m) => Runtime m [(Key, Events)]
broadcastEOF = do { (_, m) <- get
                  ; broadcast (M.keys m)
                  }
  where broadcast []     = return []
        broadcast (k:xs) = liftM2 (:) (fmap (k,) (multiplex k eofChunk)) (broadcast xs)
