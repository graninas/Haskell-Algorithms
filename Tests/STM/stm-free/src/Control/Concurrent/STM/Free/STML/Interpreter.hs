module Control.Concurrent.STM.Free.STML.Interpreter where

import           Control.Monad.Free
import           Control.Monad.State.Strict       (StateT, evalStateT, get,
                                                   modify, put)

import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar
--
-- data Runtime = Runtime
--
--
-- type STM' a = StateT Runtime IO a
--
-- newTVar' :: a -> STM' (TVar a)
-- newTVar' = error "not implemented"
--
-- readTVar' :: TVar a -> STM' a
-- readTVar' = error "not implemented"
--
-- writeTVar' :: TVar a -> a -> STM' ()
-- writeTVar' = error "not implemented"
--
--
-- interpreter' :: STMF a -> STM' a
--
-- interpreter' (NewTVar a nextF) = do
--   tvar <- newTVar' a
--   pure $ nextF tvar
--
-- interpreter' (ReadTVar tvar nextF) = do
--   a <- readTVar' tvar
--   pure $ nextF a
--
-- interpreter' (WriteTVar tvar a next) = do
--   writeTVar' tvar a
--   pure next
--
-- runSTML' :: STML a -> STM' a
-- runSTML' = foldFree interpreter'
--
-- runSTML :: STML a -> IO a
-- runSTML stm = evalStateT (runSTML' stm) Runtime
