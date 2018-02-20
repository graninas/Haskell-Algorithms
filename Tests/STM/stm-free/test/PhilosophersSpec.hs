module PhilosophersSpec where

import           Control.Concurrent.STM.Free

import           Philosophers.Philosophers
import           Philosophers.STM
import           Philosophers.Types
import           Test.Hspec

mkFork :: Int -> STML TFork
mkFork n = newTVar $ Fork (show n) Free

testFork :: STM (Bool, Bool)
testFork = do
  fork1   <- atomically $ mkFork 1
  result1 <- atomically $ takeFork fork1
  result2 <- atomically $ takeFork fork1
  pure (result1, result2)


spec =
  describe "Philosophers test" $
    it "Philosophers test" $ do

      stm <- testFork
      res <- runSTM stm

      res1 `shouldBe` Fork "1" Free

      putStrLn "Ok."
