module Control.Concurrent.STM.Free.Internal.STML.Interpreter where

import           Control.Concurrent.MVar                    (MVar, newMVar,
                                                             putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.State.Strict                 (StateT, evalStateT,
                                                             get, modify, put)
import           Data.Aeson                                 (FromJSON, ToJSON,
                                                             decode, encode)
import qualified Data.Aeson                                 as A
import qualified Data.ByteString.Lazy                       as BSL
import qualified Data.Map                                   as Map
import           GHC.Generics                               (Generic)

import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

-- TODO: this is the first implementation that is know to be wrong from STM point of view.

newTVar' :: ToJSON a => a -> STM' (TVar a)
newTVar' a = do
  mvar <- liftIO $ newMVar $ encode a
  StmlRuntime tmvarsMap <- get
  let nextId = Map.size tmvarsMap
  let newTMVars = Map.insert nextId mvar tmvarsMap
  put $ StmlRuntime newTMVars
  pure $ TVar nextId

readTVar' :: FromJSON a => TVar a -> STM' a
readTVar' (TVar tvarId) = do
  StmlRuntime tmvarsMap <- get
  case Map.lookup tvarId tmvarsMap of
    Nothing   -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just mvar -> do
      s <- liftIO $ takeMVar mvar      -- TODO: Blocking operation. Should be not blocking.
      case decode s of
        Nothing -> error $ "Decode error of TVar: " ++ show tvarId
        Just r  -> pure r

writeTVar' ::  ToJSON a => TVar a -> a -> STM' ()
writeTVar' (TVar tvarId) a = do
  StmlRuntime tmvarsMap <- get
  case Map.lookup tvarId tmvarsMap of
    Nothing   -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just mvar -> liftIO $ putMVar mvar $ encode a  -- TODO: Blocking operation. Should be not blocking.


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
runSTML stm = evalStateT (runSTML' stm) (StmlRuntime Map.empty)
