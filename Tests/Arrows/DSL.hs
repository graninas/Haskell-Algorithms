{-# LANGUAGE DeriveFunctor #-}
module DSL where

import Data.Time
import Data.IORef
import Control.Monad.Free
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

data Procedure a
    = ReadTemperature Controller (Temperature -> a)
    | Report Value a
    | Store Value a
    | AskStatus Controller (Status -> a)
    | InitBoosters (Controller -> a)
    | HeatUpBoosters Controller Power Duration a
  deriving (Functor)

type Script a = Free Procedure a

readTemperatureF c = liftF $ ReadTemperature c id
reportF v = liftF $ Report v ()
storeF v = liftF $ Store v ()
askStatusF c = liftF $ AskStatus c id
initBoostersF = liftF $ InitBoosters id
heatUpBoostersF c p d = liftF $ HeatUpBoosters c p d ()

--interpretScript :: Script a -> IO a
interpretScript (Pure a) = return a
interpretScript (Free a) = interpretProcedure a

--interpretProcedure :: Procedure a -> IO a
interpretProcedure (ReadTemperature controller next) = do
    temp <- readTemperature controller
    interpretScript (next temp)
interpretProcedure (Report v next) = do
    reportValue v
    interpretScript next
interpretProcedure (Store v next) = do
    storeValue v
    interpretScript next
interpretProcedure (AskStatus controller next) = do
    status <- askStatus controller
    interpretScript (next status)
interpretProcedure (InitBoosters next) = do
    controller <- initBoosters
    interpretScript (next controller)
interpretProcedure (HeatUpBoosters controller power dur next) = do
    heatUpBoosters controller power dur
    interpretScript next

------------ Scripts -----------------------------
reportAndStore :: Value -> Script ()
reportAndStore val = do
    reportF val
    storeF val

processTemp :: Temperature -> Script ()
processTemp t = reportAndStore (temperatureToValue t)

heatingUp :: Controller -> Script ()
heatingUp controller = do
    t1 <- readTemperatureF controller
    processTemp t1
    heatUpBoostersF controller 1.0 (seconds 10)
    t2 <- readTemperatureF controller
    processTemp t2

testBoostersScript :: Script Controller
testBoostersScript = do
    cont <- initBoostersF
    heatingUp cont
    return cont

--------------------------------------------------
------------ Scenarios ---------------------------

data Action b a = Times Int Int (Scenario b a) (b -> a)
                | EvalScript (Script b) (b -> a)
                | PrintValues [Value] a
  deriving (Functor)
  
type Scenario b a = Free (Action b) a

timesF n dt scn = liftF $ Times n dt scn id
evalScriptF scr = liftF $ EvalScript scr id
printValuesF vs = liftF $ PrintValues vs ()

--interpretScenario :: Scenario a -> IO a
interpretScenario (Pure a) = return a
interpretScenario (Free a) = interpretAction a

{-
interpretTimes' rs 0 _  _   next = interpretScenario next
interpretTimes' rs n dt scn next = do
    r <- interpretScenario scn
    threadDelay dt
    interpretTimes' (r:rs) (n-1) dt scn next
    -}

interpretTimes' = undefined
    
--interpretAction :: Action a -> IO a
interpretAction (Times n dt scn next) = do
    putStrLn $ "Times: n = " ++ show n ++ ", d = " ++ show dt
    interpretTimes' [] n dt scn next
interpretAction (EvalScript script next) = do
    putStrLn "EvalScript"
    vals <- interpretScript script
    interpretScenario (next vals)
interpretAction (PrintValues vals next) = do
    putStrLn "PrintValues"
    mapM_ print vals
    interpretScenario next

heatOnceASecond = do
    --evalScriptF testBoostersScript
    --vals <- evalScriptF readTemperatureF
    printValuesF []

testBoostersScenario = timesF 10 (seconds 1) heatOnceASecond

test = interpretScenario testBoostersScenario

----- Hardware ---------------------------------------------------------------------

initBoosters = do
   controllerImpl <- newIORef (ControllerImpl "" Online 0 (map fromIntegral [1..]))
   return $ ControllerMock controllerImpl

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
