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

mConst mf = ArrEff (\_ -> do
    c <- mf
    return (c, mConst mf))

aConst c = arr (const c)
    
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

runArrEff1 :: ArrEff IO b c -> b -> IO (c, ArrEff IO b c)
runArrEff1 (ArrEff f) b = f b

readTemperatureA :: ArrEff IO Controller Temperature
readTemperatureA = mArr $ \contr -> readTemperature contr

heatUpBoostersA :: ArrEff IO Controller Controller
heatUpBoostersA = mArr $ \contr -> heatUpBoosters contr 0 0 >> return contr

testA :: ArrEff IO Controller Temperature
testA = heatUpBoostersA >>> readTemperatureA

timesA' :: [c] -> Int -> ArrEff IO b c -> ArrEff IO b [c]
timesA' cs 0 _ = aConst cs
timesA' cs n ar = ArrEff (\b -> do
    (c, next) <- runArrEff1 ar b
    let r = c:cs
    return (r, timesA' r (n-1) next))

timesA :: Int -> ArrEff IO b c -> ArrEff IO b [c]
timesA = timesA' []

testEffectfulArrow = do
    result1 <- runArrEff (mConst initBoosters' >>> testA) [1,2,3]
    print result1 -- [2.0, 2.0, 2.0]
    
    r <- runArrEff1 (mConst initBoosters' >>> timesA 3 testA) ()
    print (fst r)
    --print result2
    
    
        
        

