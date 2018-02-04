module Lib
    ( testDiningPhilosophers
    ) where

import Control.Monad
import Control.Concurrent.STM

data ForkState = Free | Taken
  deriving Show

data Fork = Fork String ForkState
  deriving Show

data Action = Thinking | Eating
data Philosopher = Philosopher
  { action    :: Action
  , leftFork  :: TVar Fork
  , rightFork :: TVar Fork
  }

type ForkPair = (TVar Fork, TVar Fork)

-- takeFork :: TVar Fork -> STM ()
-- takeFork fork = do
--   state <- readTVar fork
--   case state of
--     Taken -> retry
--     Free  -> writeTVar fork Taken

mkFork :: Int -> IO (TVar Fork)
mkFork n = newTVarIO $ Fork (show n) Free

-- mkPhilosoper
--   :: (TVar Fork -> (TVar Fork, [TVar Philosopher]) -> (TVar Fork, [TVar Philosopher]))
--   ->

mkCycledPairs :: [TVar Fork] -> [ForkPair]
mkCycledPairs []  = error "No elems"
mkCycledPairs [_] = error "Only 1 elem"
mkCycledPairs fs  = map mkPair pairIndexes
  where
    pairIndexes :: [(Int, Int)]
    pairIndexes = [(x, x + 1) | x <- [0..length fs - 2]] ++ [(length fs - 1, 0)]
    mkPair :: (Int, Int) -> ForkPair
    mkPair (i1, i2) = (fs !! i1, fs !! i2)

printForkPair :: ForkPair -> IO ()
printForkPair (f1Var, f2Var) = do
  fs <- atomically $ do
    f1 <- readTVar f1Var
    f2 <- readTVar f2Var
    pure (f1, f2)
  putStrLn $ "\n" ++ show fs

testDiningPhilosophers :: IO ()
testDiningPhilosophers = do
  let count = 5

  forks@(f:fs) <- sequence $ take count (map mkFork [1..])
  let forkPairs = mkCycledPairs forks

  mapM_ printForkPair forkPairs

  putStrLn "Ok."
