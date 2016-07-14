{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad.State
import Control.Monad
import System.Random

data Context = Context { ctxNextId1 :: State Context Int
                       , ctxNextId2 :: State Context Int }


-- Client code. Knows nothing about random gens, but uses external state.
getNextId1 = get >>= ctxNextId1
getNextId2 = get >>= ctxNextId2

worker :: State Context [Int]
worker = do
    n1 <- getNextId1
    n2 <- getNextId1
    n3 <- getNextId2
    n4 <- getNextId2
    return [n1, n2, n3, n4]


-- The state, which will be injected into client code.
nextId :: Int -> State Context Int
nextId prevId = do let nId = prevId + 1
                   ctx <- get
                   put $ ctx { ctxNextId1 = nextId nId
                             , ctxNextId2 = nextId nId
                             }
                   return nId

nextRnd :: StdGen -> State Context Int
nextRnd prevG = do let (r, g) = random prevG
                   ctx <- get
                   put $ ctx { ctxNextId1 = nextRnd g
                             , ctxNextId2 = nextRnd g
                             }
                   return r



tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    print "Just increment:"
    let nextIdF = nextId 0
    print $ evalState worker (Context nextIdF nextIdF)
    
    print "Random Id:"
    let g = mkStdGen 100
    let nextRndF = nextRnd g
    print $ evalState worker (Context nextRndF nextRndF)