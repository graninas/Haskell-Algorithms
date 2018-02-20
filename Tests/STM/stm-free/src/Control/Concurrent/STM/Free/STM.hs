module Control.Concurrent.STM.Free.STM where

import           Control.Monad.Free

import           Control.Concurrent.STM.Free.Interpreter
import           Control.Concurrent.STM.Free.STML

data StmModelL next where
  Atomically :: STML a -> (a -> next) -> StmModelL next

type STM a = Free StmModelL a

atomically :: STML a -> STM a
atomically l = liftF (Atomically l id)
