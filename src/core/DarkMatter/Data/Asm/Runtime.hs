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
       ( Pipeline
       , Runtime
       , pipeline
       , databusMatcher
       , newMultiplex
       , multiplex
       , proc
       , exec
       , evalStateT
       ) where

import qualified Data.Map as M
import           Control.Monad.Trans
import           Control.Applicative
import           Control.Monad.Trans.State
import           DarkMatter.Data.Proc
import           DarkMatter.Data.Event
import           DarkMatter.Data.ProcLib
import           DarkMatter.Data.Asm.Types

type Pipeline = Proc Event (Maybe Event)

type Runtime m a = StateT (Pipeline, M.Map Key Pipeline) m a

procLiftM :: Proc Double Double -> Proc Event (Maybe Event)
procLiftM p = Auto f
  where f e = let (v, p1) = unAuto p (val e)
              in (Just $ update id (const v) e, procLiftM p1)

procLiftM2 :: Proc Double (Maybe Double) -> Proc Event (Maybe Event)
procLiftM2 p = Auto f
  where f e = let (v, p1) = unAuto p (val e)
              in (liftA (flip (update id) e . const) v, procLiftM2 p1)

asyncFunc :: AsyncFunc -> Pipeline
asyncFunc (Window n f)       = let func = foldr1 pipe . map (syncFunc id)
                               in procLiftM2 (window n (func f))
asyncFunc (SMA n)            = procLiftM2 (sma n)
asyncFunc (Sample n m)       = procLiftM2 (sample n m)

int2double :: Integer -> Double
int2double = fromIntegral

syncFunc :: (Proc Double Double -> Proc a b) -> SyncFunc -> Proc a b
syncFunc f Sum                = f $ binary (+)
syncFunc f Id                 = f $ proc id
syncFunc f Prod               = f $ binary (*)
syncFunc f Mean               = f $ mean
syncFunc f Maximum            = f $ binary max
syncFunc f Minimum            = f $ binary min
syncFunc f Abs                = f $ proc abs
syncFunc f Ceil               = f $ proc (int2double . ceiling)
syncFunc f Floor              = f $ proc (int2double . floor)
syncFunc f Round              = f $ proc (int2double . round)
syncFunc f Truncate           = f $ proc (int2double . truncate)
syncFunc f (Arithmetic Div t) = f $ proc (/ t)
syncFunc f (Arithmetic Sub t) = f $ proc (flip (-) t)
syncFunc f (Arithmetic Mul t) = f $ proc (* t)
syncFunc f (Arithmetic Add t) = f $ proc (+ t)
syncFunc _ _                  = error "syncFunc: unsupported operation"

-- | Creates a pipeline from a set of functions. The function list
-- must no be null.
pipeline :: [Function] -> Pipeline
pipeline = foldr1 pipeM2 . map func
  where func (Left f)  = asyncFunc f
        func (Right f) = syncFunc procLiftM f

getenv :: (Monad m) => Key -> Runtime m Pipeline
getenv k = do { (z, m) <- get
              ; case M.lookup k m
                of Nothing -> return z
                   Just y  -> return y
              }

putenv :: (Monad m) => Key -> Pipeline -> Runtime m ()
putenv k p = do { (z, m) <- get
                ; put (z, M.insert k p m)
                }

exec :: (IO (Maybe [(Key, Event)])) -> ((Key, Event) -> IO ()) -> Runtime IO ()
exec getI putO = do { mi <- liftIO getI
                    ; case mi
                      of Nothing
                           -> return ()
                         Just value
                           -> do { mapM_ (\(k, e) -> multiplex k e >>= liftIO . mputO k) value
                                 ; exec getI putO
                                 }
                    }
  where mputO _ Nothing  = return ()
        mputO k (Just e) = putO (k, e)

databusMatcher :: (Key -> Bool) -> [(Key, Event)] -> Maybe [(Key, Event)]
databusMatcher f xs = case (filter (f . fst) xs)
                      of [] -> Nothing
                         ys -> Just ys

-- | Initial state of the multiplex monad. Useful in conjunction with
 -- evalStateT.
newMultiplex :: [Function] -> (Pipeline, M.Map Key Pipeline)
newMultiplex p = (pipeline p, M.empty)

-- | Process an event and returns its output. The key is used to
-- multiplex events.
multiplex :: (Monad m) => Key -> Event -> Runtime m (Maybe Event)
multiplex k i = do { p <- getenv k
                   ; case (unAuto p i)
                     of (Nothing, p1)
                          -> putenv k p1 >> return Nothing
                        (o, p1)
                          -> putenv k p1 >> return o
                   }
