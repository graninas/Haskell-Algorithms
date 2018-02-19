module Control.Concurrent.STM.Free.STM where

import           Control.Concurrent.STM.Free.Interpreter
import           Control.Concurrent.STM.Free.STML

atomically :: STML a -> IO a
atomically l = undefined
