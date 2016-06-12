{-# LANGUAGE Arrows #-}
module ArrowsTest where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

-- This is actually a stateful stream transducer.
-- See http://stackoverflow.com/questions/4191424/what-are-arrows-and-how-can-i-use-them
newtype MyArr b c = MyArr (b -> (c, MyArr b c))

countA :: MyArr b Int
countA = count' 0
  where
    count' n = MyArr (\_ -> (n+1, count' (n+1)))

showA :: Show b => MyArr b String
showA = MyArr (\b -> (show b, showA))

runArrLst :: MyArr b c -> [b] -> [c]
runArrLst _ [] = []
runArrLst (MyArr f) (b:bs) = let (c, next) = f b
                             in c : runArrLst next bs
                            
instance Category MyArr where
    id = MyArr (\b -> (b, id))
    MyArr g . MyArr f = MyArr arrFG
      where
        arrFG a = feededF (f a)
        feededF (b, arr1) = feededG arr1 (g b)
        feededG arr1 (c, arr2) = (c, arr2 . arr1)

instance Arrow MyArr where
    arr f = MyArr (\b -> (f b, arr f))
    first (MyArr f) = MyArr arrF
      where
        arrF (b, d) = feededF (f b) d
        feededF (c, arr1) d = ((c, d), first arr1)

composedA = showA . countA

composedA' = proc lst -> do
    cnts <- countA -< lst
    shs  <- showA  -< cnts
    returnA -< shs

test1 = do
    let charsList = ['a'..'z']
    let rs1 = runArrLst showA $ runArrLst countA charsList
    putStrLn $ show rs1
    
    let rs2 = runArrLst composedA charsList
    putStrLn $ show rs2
    
    let rs3 = runArrLst composedA' charsList
    putStrLn $ show rs3
    
    putStrLn $ show $ rs1 == rs2
    putStrLn $ show $ rs1 == rs3


newtype SF b c = SF { runSF :: [b] -> [c] }

instance Category SF where
    id = SF id
    SF g . SF f = SF (g . f)

instance Arrow SF where
    arr f = SF (map f)
-- http://stackoverflow.com/questions/28402932/haskell-arrow-delay-function
    first (SF f) = SF (unzip >>> first f >>> uncurry zip)
{-
    first (SF f) = SF (uncurry zip . (f .*. id) . unzip)
      where
            (.*.) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
            (.*.) f g (a,c) = (f a, g c)
-}

delay x = SF (x:)

pairPred = arr id &&& delay 0

test2 = do
    print $ runSF (arr (+1)) [1..5]
    print $ runSF (delay 0) [1..5]
    print $ runSF pairPred [1..5]
        
        
        
        
        

