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
import qualified Data.Map                                              as Map
import           Data.Time.Clock                                       (UTCTime, getCurrentTime)
import           GHC.Generics                                          (Generic)

import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STM
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar



interpretStmModelF :: StmModelF a -> STM' a

interpretStmModelF (Atomically stml nextF) = do
  StmRuntime tmvars <- get
  timestamp <- liftIO getCurrentTime
  a <- liftIO $ evalStateT (runSTML' stml) (AtomicRuntime timestamp tmvars)
  pure $ nextF a

runSTM' :: STM a -> STM' a
runSTM' = foldFree interpretStmModelF

runSTM :: STM a -> IO a
runSTM stm = evalStateT (runSTM' stm) (StmRuntime Map.empty)
