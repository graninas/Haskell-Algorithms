{-# LANGUAGE LambdaCase #-}
module Control.Concurrent.STM.Free.Internal.STML.Interpreter where

import           Control.Concurrent.MVar                    (MVar, newMVar,
                                                             putMVar, readMVar,
                                                             takeMVar,
                                                             tryReadMVar)
import           Control.Monad.Free
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.State.Strict                 (StateT, evalStateT,
                                                             get, modify, put)
import           Data.Aeson                                 (FromJSON, ToJSON,
                                                             decode, encode)
import qualified Data.Aeson                                 as A
import qualified Data.ByteString.Lazy                       as BSL
import qualified Data.Map                                   as Map
import           Data.Time.Clock                            (UTCTime,
                                                             getCurrentTime)
import           GHC.Generics                               (Generic)

import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

-- TODO: this is the first implementation that is know to be wrong from STM point of view.

-- TODO: reading TVar - at any time is successful operation or it should be aware of TVar changing in other transactions?

createTVar :: ToJSON a => UTCTime -> Int -> a -> IO TVarHandle
createTVar timestamp tvarId a = do
   mvar <- newMVar $ encode a
   pure $ TVarHandle tvarId timestamp mvar

newTVar' :: ToJSON a => a -> STML' (TVar a)
newTVar' a = do
  AtomicRuntime timestamp tvars <- get

  let nextId = Map.size tvars
  tvarHandle <- liftIO $ createTVar timestamp nextId a

  let newTvars = Map.insert nextId tvarHandle tvars
  put $ AtomicRuntime timestamp newTvars
  pure $ TVar nextId

readTVar' :: FromJSON a => TVar a -> STML' a
readTVar' (TVar tvarId) = do
  AtomicRuntime timestamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                                -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle tvarId tvarTime mvar) -> liftIO $ tryReadMVar mvar >>= \case
      Nothing -> error $ "Impossible: Reading of MVar gave Nothing: " ++ show tvarId
      Just s  -> case decode s of
        Nothing -> error $ "Impossible: Decode error of TVar: " ++ show tvarId
        Just r  -> pure r

writeTVar' ::  ToJSON a => TVar a -> a -> STML' ()
writeTVar' (TVar tvarId) a = do
  AtomicRuntime timestamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                                -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle tvarId tvarTime mvar) -> liftIO $ putMVar mvar $ encode a  -- TODO: Blocking operation. Should be not blocking.


interpretStmf :: STMF a -> STML' a

interpretStmf (NewTVar a nextF) = do
  tvar <- newTVar' a
  pure $ nextF tvar

interpretStmf (ReadTVar tvar nextF) = do
  a <- readTVar' tvar
  pure $ nextF a

interpretStmf (WriteTVar tvar a next) = do
  writeTVar' tvar a
  pure next

runSTML' :: STML a -> STML' a
runSTML' = foldFree interpretStmf
