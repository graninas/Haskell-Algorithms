module PhilosophersSpec where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.Free

import           Philosophers.Philosophers
import           Philosophers.STM
import           Philosophers.Types
import           Test.Hspec

mkFork' :: Int -> STML TFork
mkFork' n = newTVar $ Fork (show n) Free

testFork :: Context -> IO (Bool, Bool)
testFork ctx = do
  fork1   <- atomically ctx $ mkFork' 1
  result1 <- atomically ctx $ takeFork fork1
  result2 <- atomically ctx $ takeFork fork1
  pure (result1, result2)

spec = do
  describe "STM test" $ do
    it "newTVar / readTVar test" $ do
      ctx <- newContext
      res <- atomically ctx (newTVar (10 :: Int) >>= readTVar)
      res `shouldBe` 10

    it "newTVar / writeTVar / readTVar test" $ do
      ctx <- newContext
      res <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        writeTVar tvar 20
        readTVar tvar
      res `shouldBe` 20


  describe "Philosophers test" $
    it "Philosophers test" $ do
      ctx <- newContext
      (res1, res2) <- testFork ctx

      res1 `shouldBe` True
      res2 `shouldBe` False

      putStrLn "Ok."
