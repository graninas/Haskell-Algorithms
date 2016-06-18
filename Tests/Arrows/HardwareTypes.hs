module HardwareTypes where


import Data.IORef
import Data.Time

type Name = String
type Duration = DiffTime
data Status = Online | Offline
type Power = Float
type Temperature = Float

data ControllerImpl = ControllerImpl Name Status Int [Temperature]
data Controller = ControllerMock (IORef ControllerImpl)