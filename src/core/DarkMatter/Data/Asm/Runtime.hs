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

-- | Provides the primitives necessary to execute the ASM data type.
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
       , passthrough
       , window
       , evalStateT
       ) where

import           Prelude hiding (null)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Function (on)
import           Data.Monoid (mempty)
import           Data.List (foldl1', minimumBy, maximumBy)
import qualified Data.Map as M
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Time
import           DarkMatter.Data.Event
import           DarkMatter.Data.Asm.Types

type Events = ChunkC [Event]

type Pipeline = Proc Events Events

type Runtime m a = StateT (Pipeline, M.Map Key Pipeline) m a

-- | A map-like combinator to create a pipeline.
mproc :: (Event -> Event) -> Proc Events Events
mproc f = pureF g
  where g c = fmap (map f) c

-- | A combinator useful to implement functions that change/reduce the
-- number of items, like average or maximum.
fproc :: (Event -> a)
      -- ^ Transforms the event
      -> (a -> a -> a)
      -- ^ Combine intermediate results
      -> (a -> [Event])
      -- ^ Build the response
      -> Proc Events Events
fproc f g h = await go0
  where go0 c
          | null c    = await go0
          | otherwise = let s0 = head (chk c)
                            s1 = fmap tail c
                        in go (f s0) s1
    
        go acc0 c
          | eof c     = if (null c)
                        then done (new (h acc0) True)
                        else done (new (h tmp) True)
          | null c    = await (go acc0)
          | otherwise = tmp `seq` (await (go tmp))
            where tmp = let acc1 = foldl1' g (map f (chk c))
                        in (g acc0 acc1)

-- | Utility function to unpack an event
decomp :: Event -> (Time, Double)
decomp e = (time e, val e)

-- | Use two functions to perform the cartesian product.
compose :: (a -> a -> a) -> (b -> b -> b) -> (a, b) -> (a, b) -> (a, b)
compose f g (t0, v0) (t1, v1) = t0 `seq` v0 `seq` (f t0 t1, g v0 v1)

-- | Utility function ot pack a single event
build :: (Time, Double) -> [Event]
build (t, d) = [temporal t d]

proc :: Function -> Pipeline
proc Sum                = fproc (decomp)
                                (compose (min) (+))
                                (build)
proc Prod               = fproc (decomp)
                                (compose (min) (*))
                                (build)
proc Mean               = fproc (\e -> (decomp e, 1))
                                (\(v0, n0) (v1, n1) -> v0 `seq` n0 `seq` (compose min (+) v0 v1, n0+n1))
                                (\((t, v), n) -> build (t, v/n))
proc Id                 = mproc (update id id)
proc Maximum            = fproc id (\a b -> maximumBy (compare `on` val) [a, b]) (:[])
proc Minimum            = fproc id (\a b -> minimumBy (compare `on` val) [a, b]) (:[])
proc Median             = error "todo: fixme"
proc Abs                = mproc (update id abs)
proc Ceil               = mproc (update id (fromInteger . ceiling))
proc Floor              = mproc (update id (fromInteger . floor))
proc Round              = mproc (update id (fromInteger . round))
proc Truncate           = mproc (update id (fromInteger . truncate))
proc (Arithmetic Div t) = mproc (update id (/ t))
proc (Arithmetic Sub t) = mproc (update id (flip (-) t))
proc (Arithmetic Mul t) = mproc (update id (* t))
proc (Arithmetic Add t) = mproc (update id (+ t))

-- | Creates a pipeline from a set of functions. The function list
-- must no be null.
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

passthrough :: (IO (Maybe (Key, Event))) -> (Key -> Events -> IO ()) -> Runtime IO ()
passthrough getI putO = do { mi <- liftIO getI
                           ; case mi
                             of Nothing    -> broadcastEOF >>= liftIO . mapM_ (uncurry putO)
                                Just (k,e) -> do { multiplex1 k e >>= liftIO . putO k
                                                 ; passthrough getI putO
                                                 }
                           }

window :: Int -> Int -> (IO (Maybe (Key, Event))) -> (Key -> Events -> IO ()) -> Runtime IO ()
window n m getI putO = go M.empty
  where go buffer =
          do { mi <- liftIO getI
             ; case mi
               of Nothing
                    -> do { mapM_ (\(k, e) -> flush k (new_ e)) (M.toList buffer)
                          ; broadcastEOF >>= liftIO . mapM_ (uncurry putO)
                          }
                  Just (k, e)
                    -> let (acc, bf) = insert k e buffer
                       in if (length acc == n)
                          then do { flush k (new acc True)
                                  ; go (M.insert k (take (n-m) acc) bf)
                                  }
                          else go bf
             }
        
        insert k e buffer = case (M.insertLookupWithKey (\_ [x] xs -> x:xs) k [e] buffer)
                            of (Nothing, bf)  -> ([e], bf)
                               (Just acc, bf) -> (e:acc, bf)
        
        flush k e
          | null e    = return ()
          | otherwise = multiplex k e >>= liftIO . putO k

-- | Initial state of the multiplex monad. Useful in conjunction with
-- evalStateT.
newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

-- | Singleton version of multiplex.
multiplex1 :: (Monad m) => Key -> Event -> Runtime m Events
multiplex1 k = multiplex k . new_ . (:[])

-- | Allows executing the same Pipeline over a set of different
-- events. Uses the Key to differentiate each event.
multiplex :: (Monad m) => Key -> Events -> Runtime m Events
multiplex k i = do { (z, p) <- getenv k
                   ; case (eval $ feed p i)
                     of Left f        
                          -> putenv k f >> return mempty
                        Right o
                          -> putenv k z >> return o
                   }

-- | Maps over all current pipelines sending a eofChunk them to
-- terminate any pending processing.
broadcastEOF :: (Monad m, Functor m) => Runtime m [(Key, Events)]
broadcastEOF = do { (_, m) <- get
                  ; broadcast (M.keys m)
                  }
  where broadcast []     = return []
        broadcast (k:xs) = liftM2 (:) (fmap (k,) (multiplex k eofChunk)) (broadcast xs)
