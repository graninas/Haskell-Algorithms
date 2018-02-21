{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.IORef                 (IORef, modifyIORef, newIORef,
                                             readIORef, writeIORef)
import qualified Data.Map                   as Map
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           GHC.Generics               (Generic)

type TVarId = Int

type TVarData   = IORef BSL.ByteString
data TVarHandle = TVarHandle TVarId UTCTime TVarData
type TVars      = Map.Map TVarId TVarHandle

data AtomicRuntime = AtomicRuntime
  { timestamp  :: UTCTime
  , localTVars :: TVars
  }

type Atomic a = StateT AtomicRuntime IO a

type Lock = MVar ()

data Context = Context
  { lock  :: Lock     -- TODO: lock for groups of TVars, not for all of them
  , tvars :: TVars
  }
