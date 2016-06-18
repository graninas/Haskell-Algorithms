{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module DSL where

import qualified Hardware as H
import HardwareTypes

import Control.Monad.Free

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

heatingUpTest :: Controller -> Script ()
heatingUpTest controller = do
    t1 <- readTemperature controller
    processTemp t1
    heatUpBoosters controller 1.0 (seconds 10)
    t2 <- readTemperature controller
    processTemp t2

testBoostersScript :: Script Controller
testBoostersScript = do
    cont <- initBoosters
    heatingUpTest cont
    return cont

--------------------------------------------------
------------ Scenarios ---------------------------

data Action a = forall b . Times Int (Script b) ([b] -> a)
              | forall b . EvalScript (Script b) (b -> a)
              | PrintValues [Value] a

type Scenario a = Free Action a
              
instance Functor Action where
    fmap f (PrintValues vs a) = PrintValues vs (f a)
    fmap f (EvalScript scr g) = EvalScript scr (f . g)
    fmap f (Times n scr g)    = Times n scr (f . g)

printValues vs = liftF $ PrintValues vs ()
evalScript scr = liftF $ EvalScript scr id
times n scr    = liftF $ Times n scr id

scnenario1 :: Scenario ()
scnenario1 = do
    let v1 = IntValue 1
    let v2 = IntValue 2
    printValues [v1, v2]
    evalScript (reportAndStore v1)
    cont <- evalScript initBoosters
    t1 <- evalScript (readTemperature cont)
    evalScript (heatUpBoosters cont 1.0 (seconds 10))
    t2 <- evalScript (readTemperature cont)
    printValues [temperatureToValue t1, temperatureToValue t2]

scenario2 :: Controller -> Scenario Temperature
scenario2 cont = evalScript (readTemperature cont)

heatingUp cont = do
    heatUpBoosters cont 1.0 (seconds 10)
    readTemperature cont

scenario3 = do
    cont <- evalScript initBoosters
    temps <- times 3 (heatingUp cont)
    printValues $ map temperatureToValue temps

interpretScenario (Pure a) = return a
interpretScenario (Free a) = interpretAction a

interpretAction (PrintValues vals next) = do
    putStrLn "PrintValues"
    mapM_ print vals
    interpretScenario next
interpretAction (EvalScript script next) = do
    putStrLn "EvalScript"
    vals <- interpretScript script
    interpretScenario (next vals)
interpretAction (Times n scr next) = do
    putStrLn "Times"
    let scrs = replicate n (interpretScript scr)
    vals <- sequence scrs
    interpretScenario (next vals)
   
------------ Evaluation (interpretation) of scenarios ---------------
    
testFreeOfFree :: IO ()
testFreeOfFree = do
    interpretScenario scenario3
    
----- Service functions --------------------------------------------

seconds n = n * 1000000
temperatureToValue = FloatValue

