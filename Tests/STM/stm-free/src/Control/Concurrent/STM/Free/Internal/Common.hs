module Control.Concurrent.STM.Free.Internal.Common where

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
import           Data.IORef                                 (IORef, modifyIORef,
                                                             newIORef,
                                                             readIORef,
                                                             writeIORef)
import qualified Data.Map                                   as Map
import           Data.Time.Clock                            (UTCTime,
                                                             getCurrentTime)
import           GHC.Generics                               (Generic)

import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STM
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar



takeLock :: Lock -> IO ()
takeLock = takeMVar

releaseLock :: Lock -> IO ()
releaseLock m = putMVar m ()
