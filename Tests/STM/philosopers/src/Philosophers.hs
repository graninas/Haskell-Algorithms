module Philosophers where

import           Control.Concurrent
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM
import           Control.Monad
import           System.Random           (randomRIO)

import           Philosophers.Log
import           Philosophers.Snapshot
import           Philosophers.STM
import           Philosophers.Types

mkFork :: Int -> IO TFork
mkFork n = newTVarIO $ Fork (show n) Free

mkPhilosoper :: (Int, TForkPair) -> IO Philosopher
mkPhilosoper (n, tFs) = do
  tAct    <- newTVarIO Thinking
  tCycles <- newTVarIO 0
  pure $ Philosopher (show n) tCycles tAct tFs

mkCycledPairs :: [TFork] -> [TForkPair]
mkCycledPairs []  = error "No elems"
mkCycledPairs [_] = error "Only 1 elem"
mkCycledPairs fs  = map mkPair pairIndexes
  where
    pairIndexes :: [(Int, Int)]
    pairIndexes = [(x, x + 1) | x <- [0..length fs - 2]] ++ [(length fs - 1, 0)]
    mkPair :: (Int, Int) -> TForkPair
    mkPair (i1, i2) = (fs !! i1, fs !! i2)

monitoringWorker :: LogLock -> Snapshot -> [Philosopher] -> IO ()
monitoringWorker logLock s@(ss, n) ps = do
  threadDelay $ 1000 * 1000
  snapshot <- takeSnapshot (n + 1) ps
  if s /= snapshot
    then do
      printSnapshot logLock s
      monitoringWorker logLock snapshot ps
    else monitoringWorker logLock s ps

philosopherWorker :: LogLock -> Philosopher -> IO ()
philosopherWorker logLock p@(Philosopher n _ tAct _) = do
  t1 <- randomRIO (1, 5)
  t2 <- randomRIO (1, 5)
  let activity1Time = 1000 * 1000 * t1
  let activity2Time = 1000 * 1000 * t2

  c <- atomically $ incrementCycles p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " next cycle: " ++ show c

  act1 <- atomically $ changeActivity p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " changed activity to: " ++ show act1 ++ " for " ++ show t1 ++ " secs."
  threadDelay activity1Time

  act2 <- atomically $ changeActivity p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " changed activity to: " ++ show act2 ++ " for " ++ show t2 ++ " secs."
  threadDelay activity2Time

  philosopherWorker logLock p

runPhilosopherTread :: LogLock -> Philosopher -> IO ()
runPhilosopherTread logLock ps = void $ forkIO (philosopherWorker logLock ps)

runPhilosophers :: Int -> IO ()
runPhilosophers count = do

  forks <- sequence $ take count (map mkFork [1..])
  let forkPairs = mkCycledPairs forks
  ps <- mapM mkPhilosoper (zip [1..] forkPairs)

  logLock <- newMVar ()

  s@(ss, _) <- takeSnapshot 0 ps
  printSnapshot logLock s

  _ <- forkIO (monitoringWorker logLock (ss, 1) ps)
  mapM_ (runPhilosopherTread logLock) ps
