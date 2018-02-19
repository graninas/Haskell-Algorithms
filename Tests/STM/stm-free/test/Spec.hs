module Main where

import           Control.Concurrent.STM.Free

import           Philosophers.Philosophers
import           Philosophers.STM
import           Philosophers.Types

mkFork :: Int -> STML TFork
mkFork n = newTVar $ Fork (show n) Free


main :: IO ()
main = do

  fork1 <- atomically $ mkFork 1
  fork2 <- atomically $ mkFork 2



  putStrLn "Ok."
