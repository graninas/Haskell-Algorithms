{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( testDiningPhilosophers
    ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

data ForkState = Free | Taken
  deriving Show

data Fork = Fork String ForkState
  deriving Show

data Action = Thinking | Eating
  deriving Show

type TFork     = TVar Fork
type TForkPair = (TFork, TFork)

data Philosopher = Philosopher
  { name   :: String
  , action :: TVar Action
  , forks  :: TForkPair
  }

data PhilosopherSnapshot = PhilosopherSnapshot
  { name   :: String
  , action :: Action
  , forks  :: (Fork, Fork)
  }


-- takeFork :: TVar Fork -> STM ()
-- takeFork fork = do
--   state <- readTVar fork
--   case state of
--     Taken -> retry
--     Free  -> writeTVar fork Taken

mkFork :: Int -> IO TFork
mkFork n = newTVarIO $ Fork (show n) Free

mkPhilosoper :: (Int, TForkPair) -> IO Philosopher
mkPhilosoper (n, fs) = do
  act <- newTVarIO Thinking
  pure $ Philosopher (show n) act fs

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

printForkPair :: TForkPair -> IO ()
printForkPair tFs = do
  fs <- atomically $ readForks tFs
  putStrLn $ "\n" ++ show fs

printPhilosopher :: Philosopher -> IO ()
printPhilosopher = undefined

takeSnapshot :: Philosopher -> STM PhilosopherSnapshot
takeSnapshot (Philosopher n tAct tFs) = do
  fs  <- readForks tFs
  act <- readTVar tAct
  pure $ PhilosopherSnapshot n act fs

takeSnapshots :: [Philosopher] -> IO [PhilosopherSnapshot]
takeSnapshots ps = atomically $ mapM takeSnapshot ps

printSnapshot :: PhilosopherSnapshot -> IO ()
printSnapshot (PhilosopherSnapshot n act fs) =
  putStrLn $ "\n  [" ++ n ++ "] " ++ show act ++ ", " ++ show fs

monitoring :: [Philosopher] -> Int -> IO ()
monitoring ps n = do
  snapshots <- takeSnapshots ps
  putStrLn $ "\nSnapshot #" ++ show n ++ ":"
  mapM_ printSnapshot snapshots
  threadDelay $ 1000 * 1000
  monitoring ps (n + 1)

testDiningPhilosophers :: IO ()
testDiningPhilosophers = do
  let count = 5

  forks@(f:fs) <- sequence $ take count (map mkFork [1..])
  let forkPairs = mkCycledPairs forks
  mapM_ printForkPair forkPairs

  ps <- mapM mkPhilosoper (zip [1..] forkPairs)

  _ <- forkIO (monitoring ps 0)

  putStrLn "Ok."
