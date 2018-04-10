module Philosophers2.STM where

import           Control.Concurrent
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM
import           Control.Monad
import           System.Random           (randomRIO)

data ForkState = Free | Taken
  deriving (Eq, Show)

type TFork = TVar ForkState

data Forks = Forks
  { fork1 :: TFork
  , fork2 :: TFork
  , fork3 :: TFork
  , fork4 :: TFork
  , fork5 :: TFork
  }

takeFork :: TFork -> STM Bool
takeFork tFork = do
  forkState <- readTVar tFork
  when (forkState == Free) (writeTVar tFork Taken)
  pure (forkState == Free)

takeForks' :: (TFork, TFork) -> STM Bool
takeForks' (tLeftFork, tRightFork) = do
  leftTaken  <- takeFork tLeftFork
  rightTaken <- takeFork tRightFork
  pure (leftTaken && rightTaken)


takeForks :: (TFork, TFork) -> STM ()
takeForks (tLeftFork, tRightFork) = do
  leftTaken  <- takeFork tLeftFork
  rightTaken <- takeFork tRightFork
  when (not leftTaken || not rightTaken) retry


data PhilosopherState = Thinking | Eating

data Philosopher = Philosopher
  { pState      :: TVar PhilosopherState
  , pLeftFork   :: TFork
  , pRrightFork :: TFork
  }

changePhilosopherActivity :: Philosopher -> STM ()
changePhilosopherActivity (Philosopher tState tLeftFork tRightFork) = do
  state <- readTVar tState
  case state of
    Thinking -> error "Changing state from Thinking not implemented."
    Eating   -> do
      takeForks (tLeftFork, tRightFork)
      writeTVar tState Eating

philosoperWorker :: Philosopher -> IO ()
philosoperWorker philosopher = do
  atomically (changePhilosopherActivity philosopher)
  threadDelay 5000
  philosoperWorker philosopher

runPhilosophers :: IO ()
runPhilosophers = do
  tState1 <- newTVarIO Thinking
  tState2 <- newTVarIO Thinking
  tFork1  <- newTVarIO Free
  tFork2  <- newTVarIO Free

  forkIO (philosoperWorker (Philosopher tState1 tFork1 tFork2))
  forkIO (philosoperWorker (Philosopher tState2 tFork2 tFork1))

  threadDelay 100000
