{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module ArrowizedDSL where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free

import ArrEff
import HardwareTypes
import qualified Hardware as H
import DSL

-------------- demo processes ----------------------------------

readTemperatureA :: ArrEff IO Controller Temperature
readTemperatureA = mArr H.readTemperature

heatUpBoostersA :: ArrEff IO Controller Controller
heatUpBoostersA = mArr $ \contr -> H.heatUpBoosters contr 0 0 >> return contr

testA :: ArrEff IO Controller Temperature
testA = heatUpBoostersA >>> readTemperatureA
    
testEffectfulArrow = do
    result1 <- runArrEff (mConst H.initBoosters >>> testA) [1,2,3]
    print result1 -- [2.0, 2.0, 2.0]
    
    r1 <- runArrEff1 (mConst H.initBoosters >>> timesA 3 testA) ()
    print (fst r1) -- [2.0,3.0,4.0]
    r2 <- runArrEff1 (snd r1) ()
    print (fst r2) -- []

---------------------------------------------------------------------------
type ArrEffFreeProc = ArrEff (Free Procedure)

simpleArrFA :: ArrEffFreeProc Value ()
simpleArrFA = mArr report

initBoostersFA :: ArrEffFreeProc () Controller
initBoostersFA = mConst initBoosters

readTemperatureFA :: ArrEffFreeProc Controller Temperature
readTemperatureFA = mArr readTemperature

runFreeArr :: ArrEffFreeProc b c -> b -> IO c
runFreeArr ar v = do
    let p = runArrEff1 ar v
    (c, next) <- interpretScript p -- TODO: what to do with next?
    return c

heatingUpFA = proc cont -> do
    t1      <- readTemperatureFA                 -< cont
    r_      <- mArr (\c -> heatUpBoosters c 0 0) -< cont
    (t2, _) <- first readTemperatureFA           -< (cont, r_)
    returnA -< [t1, t2]
    
heatingUpProcessFA = proc x -> do
    cont <- initBoostersFA -< x
    ts   <- heatingUpFA    -< cont
    returnA -< ts
    
testFreeEffArrow :: IO ()
testFreeEffArrow = do
    r1 <- runFreeArr simpleArrFA (FloatValue 1.0)
    r2 <- runFreeArr heatingUpProcessFA ()
    print r1
    print r2
