module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.STM
import qualified Data.Map as M

data Gen = Gen Int (Int -> Int)

data Turn = A | B
  deriving (Eq, Show)

twoStepGen :: Int -> Gen
twoStepGen n = Gen n (\x -> x + 5)

nextGenValue :: Gen -> (Int, Gen)
nextGenValue (Gen n f) = let n' = f n in (n', (Gen n' f))

notMe A = B
notMe B = A

turn 0 _ _ = return $ Right 0
turn cnt me (turntv, gentv) = do
    v <- readTVarIO turntv
    if (v == me) then do
         g <- readTVarIO gentv
         let (v, g') = nextGenValue g
         print (me, v)
         atomically $ writeTVar gentv g'
         atomically $ writeTVar turntv (notMe me)
         return $ Right (cnt - 1)
    else
      return $ Left ()

worker doneMV cnt me tvs = do
    res <- turn cnt me tvs
    case res of
         Left () -> do
             threadDelay 50
             worker doneMV cnt me tvs
         Right 0 -> do
             putMVar doneMV ()
         Right n -> worker doneMV n me tvs

test = do
    doneMV1 <- newEmptyMVar
    doneMV2 <- newEmptyMVar
    turntv <- newTVarIO A
    gentv <- newTVarIO (twoStepGen 0)
    
    forkIO $ worker doneMV1 10 A (turntv, gentv)
    forkIO $ worker doneMV2 10 B (turntv, gentv)
    
    mapM_ readMVar [doneMV1, doneMV2]
    
    {-
    Will print:

    (A,5)
    (B,10)
    (A,15)
    (B,20)
    (A,25)
    (B,30)
    (A,35)
    (B,40)
    (A,45)
    (B,50)
    (A,55)
    (B,60)
    (A,65)
    (B,70)
    (A,75)
    (B,80)
    (A,85)
    (B,90)
    (A,95)
    (B,100)
    -}
    