{-# LANGUAGE DuplicateRecordFields #-}
module Philosophers.Snapshot where

import           Control.Concurrent
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM
import           Control.Monad

import           Philosophers.Log
import           Philosophers.STM
import           Philosophers.Types

data Shot = Shot
  { name     :: String
  , cycles   :: Int
  , activity :: Activity
  , forks    :: (Fork, Fork)
  }
  deriving Eq

type Snapshot = ([Shot], Int)

takeShot :: Philosopher -> STM Shot
takeShot (Philosopher n tC tAct tFs) = do
  c   <- readTVar  tC
  act <- readTVar  tAct
  fs  <- readForks tFs
  pure $ Shot n c act fs

takeSnapshot :: Int -> [Philosopher] -> IO Snapshot
takeSnapshot n ps = (,) <$> atomically (mapM takeShot ps) <*> pure n

printShot :: Shot -> IO ()
printShot (Shot n c act fs) = putStrLn $ "  [" ++ n ++ "] (" ++ show c ++ ") " ++ show act ++ ", " ++ show fs

printSnapshot :: LogLock -> Snapshot -> IO ()
printSnapshot logLock (s, n) = do
  acquire logLock
  putStrLn $ "Snapshot #" ++ show n ++ ":"
  mapM_ printShot s
  release logLock
