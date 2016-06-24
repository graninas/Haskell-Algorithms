module Hardware where

import HardwareTypes

import Data.IORef

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
