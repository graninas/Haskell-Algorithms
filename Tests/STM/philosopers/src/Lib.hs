{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( testDiningPhilosophers
    ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM
import System.Random (randomRIO)

data ForkState = Free | Taken
  deriving (Show, Eq)

data Fork = Fork String ForkState
  deriving (Show, Eq)

data Activity = Thinking | Eating
  deriving (Show, Eq)

type TFork     = TVar Fork
type TForkPair = (TFork, TFork)

-- TODO: With this data structure, philosopher can "put" foreign fork.
-- Forks should be peronalized.

data Philosopher = Philosopher
  { name     :: String
  , cycles   :: TVar Int
  , activity :: TVar Activity
  , forks    :: TForkPair
  }

data Shot = Shot
  { name     :: String
  , cycles   :: Int
  , activity :: Activity
  , forks    :: (Fork, Fork)
  }
  deriving Eq

type Snapshot = ([Shot], Int)

type LogLock = MVar ()

logMsg :: LogLock -> String -> IO ()
logMsg logLock msg = do
  _ <- takeMVar logLock
  putStrLn msg
  putMVar logLock ()

acquire :: LogLock -> IO ()
acquire l = void $ takeMVar l

release :: LogLock -> IO ()
release l = putMVar l ()

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

readForks :: TForkPair -> STM (Fork, Fork)
readForks (l, r) = (,) <$> readTVar l <*> readTVar r

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

monitoring :: LogLock -> Snapshot -> [Philosopher] -> IO ()
monitoring logLock s@(ss, n) ps = do
  threadDelay $ 1000 * 1000
  snapshot <- takeSnapshot (n + 1) ps
  if s /= snapshot
    then do
      printSnapshot logLock s
      monitoring logLock snapshot ps
    else monitoring logLock s ps

takeFork :: TFork -> STM Bool
takeFork tFork = do
  Fork n st <- readTVar tFork
  case st of
    Taken -> pure False
    Free  -> do
      modifyTVar' tFork (\(Fork n st) -> Fork n Taken)
      pure True

takeForks :: TForkPair -> STM Bool
takeForks (left, right) = do
  leftTaken  <- takeFork left
  rightTaken <- takeFork right
  pure $ leftTaken && rightTaken

-- N.B., Someone can "put" foreign fork.
putFork :: TFork -> STM ()
putFork tFork = modifyTVar' tFork (\(Fork n st) -> Fork n Free)

putForks :: TForkPair -> STM ()
putForks (left, right) = do
  putFork left
  putFork right

changeActivity :: Philosopher -> STM Activity
changeActivity (Philosopher n tC tAct tFs) = do
  act <- readTVar tAct
  case act of
    Thinking -> do
      taken <- takeForks tFs
      unless taken retry  -- Do not need to put forks if any was taken!
      writeTVar tAct Eating
      pure Eating
    Eating -> do
      putForks tFs
      writeTVar tAct Thinking
      pure Thinking

incrementCycles :: Philosopher -> STM Int
incrementCycles (Philosopher _ tCycles _ _) = do
  modifyTVar' tCycles (+1)
  readTVar tCycles

philosopher :: LogLock -> Philosopher -> IO ()
philosopher logLock p@(Philosopher n _ tAct _) = do
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

  philosopher logLock p

runPhilosopher :: LogLock -> Philosopher -> IO ()
runPhilosopher logLock ps = void $ forkIO (philosopher logLock ps)

testDiningPhilosophers :: IO ()
testDiningPhilosophers = do
  let count = 5

  forks <- sequence $ take count (map mkFork [1..])
  let forkPairs = mkCycledPairs forks
  ps <- mapM mkPhilosoper (zip [1..] forkPairs)

  logLock <- newMVar ()

  s@(ss, _) <- takeSnapshot 0 ps
  printSnapshot logLock s

  _ <- forkIO (monitoring logLock (ss, 1) ps)
  mapM_ (runPhilosopher logLock) ps

  putStrLn "Ok."
