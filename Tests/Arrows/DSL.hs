module DSL where

import Data.Time
import Data.IORef
import Control.Concurrent
import System.IO.Unsafe

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show)
  
type Name = String
type Duration = DiffTime
data Status = Online | Offline
type Power = Float
type Temperature = Float

data ControllerImpl = ControllerImpl Name Status Int [Temperature]
data Controller = ControllerMock (IORef ControllerImpl)

type Script = [Procedure]

data Procedure
    = ReadTemperature Controller (Temperature -> Script)
    | Report Value
    | Store Value
    | AskStatus Controller (Status -> Script)
    | InitBoosters (Controller -> Script)
    | HeatUpBoosters Controller Power Duration

evalScript :: Script -> IO [Value]
evalScript script = evalScript' script []
evalScript' :: Script -> [Value] -> IO [Value]
evalScript' [] out = return out
evalScript' (p:ps) out = do
    vals <- evalProcedure p
    evalScript' ps (out ++ vals)

evalProcedure :: Procedure -> IO [Value]
evalProcedure (ReadTemperature controller next) = do
    temp <- readTemperature controller
    evalScript (next temp)
evalProcedure (Report v) = reportValue v >> return []
evalProcedure (Store v)  = storeValue v >> return []
evalProcedure (AskStatus controller next) = do
    status <- askStatus controller
    evalScript (next status)
evalProcedure (InitBoosters next) = do
    controller <- initBoosters
    evalScript (next controller)
evalProcedure (HeatUpBoosters controller power dur) = do
    heatUpBoosters controller power dur
    return []


data Action = Times Int Int Action
            | EvalScript Script ([Value] -> Scenario)
            | PrintValues [Value]
            
type Scenario = [Action]

evalScenario :: Scenario -> IO ()
evalScenario acts = mapM_ evalAction acts

evalAction (Times n dt action) = do
    let acts = replicate n (evalAction action >> threadDelay dt)
    putStrLn $ "Times: n = " ++ show n ++ ", d = " ++ show dt
    sequence_ acts
evalAction (EvalScript script next) = do
    putStrLn "EvalScript"
    vals <- evalScript script
    evalScenario (next vals)
evalAction (PrintValues vals) = do
    putStrLn "PrintValues"
    mapM_ print vals


reportAndStore :: Value -> Script
reportAndStore val = [ Report val, Store val ]

processTemp :: Temperature -> Script
processTemp t = reportAndStore (temperatureToValue t)

heatingUp :: Controller -> Script
heatingUp controller =
    [ ReadTemperature controller processTemp
    , HeatUpBoosters controller 1.0 (seconds 10)
    , ReadTemperature controller processTemp ]

testBoostersScript :: Script
testBoostersScript = [ InitBoosters heatingUp ]

printValues :: [Value] -> Scenario
printValues vals = [ PrintValues vals ]

heatOnceASecond = EvalScript testBoostersScript printValues

testBoostersScenario :: Scenario
testBoostersScenario = [ Times 10 (seconds 1) heatOnceASecond ]


test = do
    evalScenario testBoostersScenario

-- this is a hack for demo
globalContr :: Controller
{-# NOINLINE globalContr #-}
globalContr = unsafePerformIO initBoosters'

-- This is a hack for demo
initBoosters' = do
   controllerImpl <- newIORef (ControllerImpl "" Online 0 (map fromIntegral [1..]))
   return $ ControllerMock controllerImpl

initBoosters = return globalContr

reportValue v = print ("reported: " ++ show v)
storeValue v = print ("stored: " ++ show v)
readTemperature (ControllerMock controllerImpl) = do
    ControllerImpl _ st n ts <- readIORef controllerImpl
    return (head ts)
askStatus _ = return Online
heatUpBoosters (ControllerMock controller) _ _ = do
    ControllerImpl _ st n ts <- readIORef controller
    writeIORef controller $ ControllerImpl "" st (n + 1) (drop 1 ts)
seconds n = n * 1000000
temperatureToValue = FloatValue
