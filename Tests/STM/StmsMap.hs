module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.STM
import qualified Data.Map as M

data Gen = Gen Int (Int -> Int)
         | Gen' (Int -> Int)

type Table = M.Map String (TVar Gen)

twoStepGen :: Int -> Gen
twoStepGen n = Gen n (\x -> x + 2)

twoStepGen' :: Int -> Gen
twoStepGen' n = Gen' (\x -> n + x + 2)

nextGenValue :: Gen -> (Int, Gen)
nextGenValue (Gen n f) = let n' = f n in (n', (Gen n' f))
nextGenValue (Gen' f) = let v = f 0
                            f' = twoStepGen' v
                        in (v, f')

rec 0 _ _ = return []
rec cnt k m = case M.lookup k m of
    Just tv -> do
        g <- readTVarIO tv
        let (v, g') = nextGenValue g
        atomically $ writeTVar tv g'
        vs <- rec (cnt - 1) k m
        return $ v : vs
    Nothing -> error "not found."                    

worker mv cnt k m = do
    vs <- rec cnt k m
    putMVar mv vs
    
test = do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar

    let (v1, g1) = nextGenValue (twoStepGen 10)
    let (v2, g2) = nextGenValue g1
    
    let (v1', g1') = nextGenValue (twoStepGen' 11)
    let (v2', g2') = nextGenValue g1'
    
    print [v1, v2]
    print [v1', v2']
    -- Will print:
    -- [12,14]
    -- [13,15]    
    
    t1 <- newTVarIO (twoStepGen 10)
    t2 <- newTVarIO (twoStepGen' 11)
    
    let m = M.fromList [ ("1", t1), ("2", t2) ]
    
    forkIO $ worker mv1 10 "1" m
    forkIO $ worker mv2 10 "2" m
    
    vs1 <- readMVar mv1
    vs2 <- readMVar mv2
    
    print vs1
    print vs2
    -- Will print:
    -- [12,14,16,18,20,22,24,26,28,30]
    -- [13,15,17,19,21,23,25,27,29,31]
    -- This means, both tvars are updating inside map while we haven't been updating map at all.
    