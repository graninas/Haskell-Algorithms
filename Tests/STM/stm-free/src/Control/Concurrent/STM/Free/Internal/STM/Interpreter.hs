module Control.Concurrent.STM.Free.Internal.STM.Interpreter where

import           Control.Concurrent.MVar                               (MVar,
                                                                        newMVar,
                                                                        putMVar,
                                                                        takeMVar)
import           Control.Monad.Free
import           Control.Monad.IO.Class                                (liftIO)
import           Control.Monad.State.Strict                            (StateT, evalStateT,
                                                                        get,
                                                                        modify,
                                                                        put)
import           Data.Aeson                                            (FromJSON,
                                                                        ToJSON,
                                                                        decode,
                                                                        encode)
import qualified Data.Aeson                                            as A
import qualified Data.ByteString.Lazy                                  as BSL
import           Data.IORef                                            (IORef, modifyIORef,
                                                                        newIORef,
                                                                        readIORef,
                                                                        writeIORef)
import qualified Data.Map                                              as Map
import           Data.Time.Clock                                       (UTCTime, getCurrentTime)
import           GHC.Generics                                          (Generic)

import           Control.Concurrent.STM.Free.Internal.Common
import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STM
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar


cloneTVarHandle :: (TVarId, TVarHandle) -> IO (TVarId, TVarHandle)
cloneTVarHandle (tvarId, TVarHandle _ timestamp tvarData) = do
  newTVarData <- readIORef tvarData >>= newIORef
  pure (tvarId, TVarHandle tvarId timestamp newTVarData)

takeSnapshot :: Lock -> TVars -> IO TVars
takeSnapshot lock tvars = do
  takeLock lock
  tvarKVs <- mapM cloneTVarHandle (Map.toList tvars)
  pure $ Map.fromList tvarKVs

interpretStmModelF :: StmModelF a -> STM' a

interpretStmModelF (Atomically stml nextF) = do
  StmRuntime tvars lock <- get

  snapshot  <- liftIO $ takeSnapshot lock tvars
  timestamp <- liftIO getCurrentTime
  a <- liftIO $ evalStateT (runSTML' stml) (AtomicRuntime timestamp snapshot)

  -- Check goes here
  pure $ nextF a

runSTM' :: STM a -> STM' a
runSTM' = foldFree interpretStmModelF

runSTM :: STM a -> IO a
runSTM stm = do
  commitLock <- newMVar ()
  evalStateT (runSTM' stm) (StmRuntime Map.empty commitLock)
