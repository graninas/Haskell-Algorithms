{-# LANGUAGE Arrows #-}
module Arrows where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

import DSL

-------------------------------------------------------------------------------

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

testSFArr = do
    print $ runSF (arr (+1)) [1..5]
    print $ runSF (delay 0) [1..5]
    print $ runSF pairPred [1..5]
        
-----------------------------------------------------------------------------

-- This is actually a stateful stream transducer.
-- See http://stackoverflow.com/questions/4191424/what-are-arrows-and-how-can-i-use-them
newtype MyArr b c = MyArr (b -> (c, MyArr b c))
                            
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

runArrLst :: MyArr b c -> [b] -> [c]
runArrLst _ [] = []
runArrLst (MyArr f) (b:bs) = let (c, next) = f b
                             in c : runArrLst next bs
        
countA :: MyArr b Int
countA = count' 0
  where
    count' n = MyArr (\_ -> (n+1, count' (n+1)))

showA :: Show b => MyArr b String
showA = MyArr (\b -> (show b, showA))

                             
composedA = showA . countA

composedA' = proc lst -> do
    cnts <- countA -< lst
    shs  <- showA  -< cnts
    returnA -< shs

testMyArr = do
    let charsList = ['a'..'z']
    let rs1 = runArrLst showA $ runArrLst countA charsList
    putStrLn $ show rs1
    
    let rs2 = runArrLst composedA charsList
    putStrLn $ show rs2
    
    let rs3 = runArrLst composedA' charsList
    putStrLn $ show rs3
    
    putStrLn $ show $ rs1 == rs2
    putStrLn $ show $ rs1 == rs3

------------------------------------------------------------
    
newtype ArrEff eff b c = ArrEff (b -> eff (c, ArrEff eff b c))

instance Monad eff => Category (ArrEff eff) where
    id = ArrEff (\b -> return (b, id))
    ArrEff g . ArrEff f = ArrEff arrFG
      where
        arrFG a = do
            fa <- f a
            feededF fa
        feededF (b, arr1) = do
            gb <- g b
            feededG arr1 gb
        feededG arr1 (c, arr2) = return (c, arr2 . arr1)

instance Monad eff => Arrow (ArrEff eff) where
    arr f = ArrEff (\b -> return (f b, arr f))
    first (ArrEff f) = ArrEff arrF
      where
        arrF (b, d) = do
            fb <- f b
            feededF fb d
        feededF (c, arr1) d = return ((c, d), first arr1)

instance Monad eff => Functor (ArrEff eff b) where
    fmap f (ArrEff r) = ArrEff (\b -> do
        (c, next) <- r b
        return (f c, fmap f next))

mArr mf = ArrEff (\b -> do
    c <- mf b
    return (c, mArr mf))
    
runArrEvent :: Read b => ArrEff IO b (Bool, c) -> [c] -> IO [c]
runArrEvent (ArrEff f) cs = do
    b <- getLine
    result <- f (read b)
    case result of
        ((True, c), next) -> runArrEvent next (c:cs)
        ((False, c), _)   -> return (c:cs)

runArrEff' :: [c] -> ArrEff IO b c -> [b] -> IO [c]
runArrEff' cs (ArrEff f) []     = return cs
runArrEff' cs (ArrEff f) (b:bs) = do
    (c, next) <- f b
    runArrEff' (c:cs) next bs

runArrEff :: ArrEff IO b c -> [b] -> IO [c]
runArrEff = runArrEff' []
    
readTemperatureA :: ArrEff IO b Temperature
readTemperatureA = mArr $ \_ -> readTemperature globalContr

heatUpBoostersA :: ArrEff IO b ()
heatUpBoostersA = mArr $ \_ -> heatUpBoosters globalContr 0 0

testA :: ArrEff IO () Temperature
testA = readTemperatureA <<< heatUpBoostersA

testEvent = do
--    t1 <- readTemperature globalContr
--    heatUpBoosters globalContr 0 0
--    t2 <- readTemperature globalContr
--    print t1
--    print t2

    runArrEff testA [()]
    t3 <- readTemperature globalContr
    print t3

    
    
        
        

