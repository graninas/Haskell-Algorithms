module STM.Interpreter where

import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)

import           STM.Free
import           STM.TVar

data Runtime = Runtime


type STM' a = StateT Runtime IO a

newTVar' :: a -> STM' (TVar a)
newTVar' = error "not implemented"

readTVar' :: TVar a -> STM' a
readTVar' = error "not implemented"

writeTVar' :: TVar a -> a -> STM' ()
writeTVar' = error "not implemented"


interpreter' :: STMF a -> STM' a

interpreter' (NewTVar a nextF) = do
  tvar <- newTVar' a
  pure $ nextF tvar

interpreter' (ReadTVar tvar nextF) = do
  a <- readTVar' tvar
  pure $ nextF a

interpreter' (WriteTVar tvar a next) = do
  writeTVar' tvar a
  pure next

runSTM' :: STM a -> STM' a
runSTM' = foldFree interpreter'

run :: STM a -> IO a
run stm = evalStateT (runSTM' stm) Runtime
