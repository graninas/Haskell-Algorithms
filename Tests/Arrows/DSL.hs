{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module DSL where

import qualified Hardware as H
import HardwareTypes

import Data.Time
import Data.IORef
import Control.Monad.Free
import Control.Concurrent
import System.IO.Unsafe

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show)
  
data Procedure a
    = ReadTemperature Controller (Temperature -> a)
    | Report Value a
    | Store Value a
    | AskStatus Controller (Status -> a)
    | InitBoosters (Controller -> a)
    | HeatUpBoosters Controller Power Duration a
  deriving (Functor)

type Script a = Free Procedure a

readTemperature c = liftF $ ReadTemperature c id
report v = liftF $ Report v ()
store v = liftF $ Store v ()
askStatus c = liftF $ AskStatus c id
initBoosters = liftF $ InitBoosters id
heatUpBoosters c p d = liftF $ HeatUpBoosters c p d ()

interpretScript (Pure a) = return a
interpretScript (Free a) = interpretProcedure a

interpretProcedure (ReadTemperature controller next) = do
    temp <- H.readTemperature controller
    interpretScript (next temp)
interpretProcedure (Report v next) = do
    H.reportValue v
    interpretScript next
interpretProcedure (Store v next) = do
    H.storeValue v
    interpretScript next
interpretProcedure (AskStatus controller next) = do
    status <- H.askStatus controller
    interpretScript (next status)
interpretProcedure (InitBoosters next) = do
    controller <- H.initBoosters
    interpretScript (next controller)
interpretProcedure (HeatUpBoosters controller power dur next) = do
    H.heatUpBoosters controller power dur
    interpretScript next

------------ Scripts -----------------------------
reportAndStore :: Value -> Script ()
reportAndStore val = do
    report val
    store val

processTemp :: Temperature -> Script ()
processTemp t = reportAndStore (temperatureToValue t)

heatingUp :: Controller -> Script ()
heatingUp controller = do
    t1 <- readTemperature controller
    processTemp t1
    heatUpBoosters controller 1.0 (seconds 10)
    t2 <- readTemperature controller
    processTemp t2

testBoostersScript :: Script Controller
testBoostersScript = do
    cont <- initBoosters
    heatingUp cont
    return cont

--------------------------------------------------
------------ Scenarios ---------------------------

data Action a = --Times Int Int (ScenarioFT b a) (b -> a)
                forall b .EvalScript (Script b) (b -> a)
              | PrintValues [Value] a

instance Functor Action where
    fmap f (PrintValues vs a) = PrintValues vs (f a)
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

printValues vs = liftF $ PrintValues vs ()
evalScript scr = liftF $ EvalScript scr id

scnenario = do
    let v1 = IntValue 1
    let v2 = IntValue 2
    printValues [v1, v2]
    evalScript (reportAndStore v1)
    cont <- evalScript initBoosters
    t1 <- evalScript (readTemperature cont)
    evalScript (heatUpBoosters cont 1.0 (seconds 10))
    t2 <- evalScript (readTemperature cont)
    printValues [temperatureToValue t1, temperatureToValue t2]

testFreeOfFree :: IO ()
testFreeOfFree = interpretScenario scnenario

interpretScenario (Pure a) = return a
interpretScenario (Free a) = interpretActionF a

interpretActionF (PrintValues vals next) = do
    putStrLn "PrintValues"
    mapM_ print vals
    interpretScenario next
interpretActionF (EvalScript script next) = do
    putStrLn "EvalScript"
    vals <- interpretScript script
    interpretScenario (next vals)

----- Service functions --------------------------------------------

seconds n = n * 1000000
temperatureToValue = FloatValue
