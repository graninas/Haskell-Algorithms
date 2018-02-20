module Control.Concurrent.STM.Free.STML.Interpreter where

import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)
import           Control.Monad.Free
import           Control.Concurrent.MVar (MVar, takeMVar, newMVar, putMVar)
import           Control.Monad.State.Strict       (StateT, evalStateT, get,
                                                   modify, put)

import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar
import Control.Concurrent.STM.Free.Internal.Types


newTVar' :: Generic a => a -> STM' (TVar a)
newTVar' a = do
  mvar <- newMVar $ encode a
  StmlRuntime tvarsMap <- get
  let nextId = Map.size tvarsMap
  let newTVars = Map.insert nextId mvar tvars
  put newTVars
  pure $ TVar nextId

readTVar' :: Generic a => TVar a -> STM' a
readTVar' (TVar tvarId) = do
  StmlRuntime tvarsMap <- get
  case Map.lookup tvarId tvarsMap of
    Nothing -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just s  -> case decode s of
      Nothing -> error $ "Decode error of TVar: " ++ show tvarId


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

runSTML' :: STML a -> STM' a
runSTML' = foldFree interpreter'

runSTML :: STML a -> IO a
runSTML stm = evalStateT (runSTML' stm) StmlRuntime
