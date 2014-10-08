{-# LANGUAGE ExistentialQuantification #-}

module Leela.Data.Pipeline
       ( Env (..)
       , Pipeline (..)
       , idEnv
       , execM
       , packM
       , runPipeline
       , runPipeline_
       , runPipelineIO
       ) where

import qualified Data.Vector as V
import           Control.Monad
import           Leela.Data.Funclib
import           Control.Monad.Identity

data Pipeline m a = forall s. Filter { env  :: Env m s
                                     , code :: Func s a a
                                     }

data Env m s = Env { getenv :: m s
                   , putenv :: s -> m ()
                   }

idEnv :: s -> Env Identity s
idEnv s = Env (return s) (const $ return ())

runPipeline :: (a -> Bool) -> [Pipeline m a] -> a -> ([Pipeline m a], a)
runPipeline accept = go []
    where
      go acc [] b                = (reverse acc, b)
      go acc (Filter e f : fs) a
        | accept a               = let (g, b) = run f a
                                   in go (Filter e g : acc) fs b
        | otherwise              = (reverse acc ++ (Filter e f : fs), a)

runPipelineIO :: (a -> IO ()) -> (a -> Bool) -> [Pipeline m a] -> a -> IO [Pipeline m a]
runPipelineIO write accept f a = do
  let (g, b) = runPipeline accept f a
  when (accept b) (write b)
  return g

runPipeline_ :: (a -> Bool) -> [Pipeline m a] -> a -> a
runPipeline_ accept pipeline = snd . runPipeline accept pipeline

execM :: (Monad m) => [Pipeline m a] -> a -> m a
execM [] a = return a
execM (Filter e f : fs) a = do
  f' <- liftM (load f) (getenv e)
  execM fs =<< save (run f' a)
    where
      save (g, b) = putenv e (dump g) >> return b

packM :: (Monad m) => Env m s -> Func s (V.Vector a) (V.Vector a) -> m (Pipeline m (V.Vector a))
packM env fun = do
  s <- getenv env
  return (Filter env (load fun s))
