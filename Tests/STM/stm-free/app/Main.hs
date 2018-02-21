module Main where

import           Control.Concurrent.STM.Free
import           Philosophers.Philosophers

main :: IO ()
main = runPhilosophers 5
