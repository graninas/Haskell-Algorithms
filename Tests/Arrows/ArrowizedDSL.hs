{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module ArrowizedDSL where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Free

import ArrEff
import HardwareTypes
import qualified Hardware as H
import DSL

-------------- demo IO processes ----------------------------------

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

------ Arrows that work with Script -------------------------------------

type ArrEffFreeProc b c = ArrEffFree Procedure b c
    
reportScriptFA :: ArrEffFreeProc Value ()
reportScriptFA = mArr report

initBoostersScriptFA :: ArrEffFreeProc () Controller
initBoostersScriptFA = mConst initBoosters

readTemperatureFA :: ArrEffFreeProc Controller Temperature
readTemperatureFA = mArr readTemperature

heatingUpScriptFA = proc cont -> do
    t1      <- readTemperatureFA                 -< cont
    r_      <- mArr (\c -> heatUpBoosters c 0 0) -< cont
    (t2, _) <- first readTemperatureFA           -< (cont, r_)
    returnA -< [t1, t2]
    
heatingUpProcessScriptFA = proc x -> do
    cont <- initBoostersScriptFA -< x
    ts   <- heatingUpScriptFA    -< cont
    returnA -< ts
    
testScriptArrow :: IO ()
testScriptArrow = do
    r1 <- runFreeArr interpretScript reportScriptFA (FloatValue 1.0)
    r2 <- runFreeArr interpretScript heatingUpProcessScriptFA ()
    print r1
    print r2

------ Arrows that work with Scenario -------------------------------------
type ArrEffFreeAct b c = ArrEffFree Action b c

printValuesScenarioFA :: ArrEffFreeAct [Value] ()
printValuesScenarioFA = mArr printValues

--evalScriptScenarioFA :: Script b -> ArrEffFreeAct () b
-- TODO: understand this.
evalScriptScenarioFA scr = mArr (evalScript . scr)

printValueScenarioFA :: ArrEffFreeAct Value ()
printValueScenarioFA = mArr printValue

heatingUpScenarioFA :: ArrEffFreeAct Controller Temperature
heatingUpScenarioFA = mArr (evalScript . heatingUpScript)

scenario3FA :: ArrEffFreeAct () [Temperature]
scenario3FA = proc x -> do
     cont <- evalScriptScenarioFA (const initBoosters) -< x
     ts <- timesA 3 heatingUpScenarioFA -< cont
     forEachA printValueScenarioFA -< map temperatureToValue ts
     returnA -< ts

testScenarioArrow :: IO ()
testScenarioArrow = do
    r1 <- runFreeArr interpretScenario printValuesScenarioFA [FloatValue 1.333, IntValue 44]
    print r1
    r2 <- runFreeArr interpretScenario scenario3FA ()
    print r2
