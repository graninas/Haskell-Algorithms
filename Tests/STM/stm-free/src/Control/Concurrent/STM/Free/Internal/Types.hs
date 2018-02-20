{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map                   as Map
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           GHC.Generics               (Generic)

type TVarId = Int
type TVarItem = MVar BSL.ByteString

data TVarHandle = TVarHandle Int UTCTime TVarItem

type TVars = Map.Map Int TVarHandle

data AtomicRuntime = AtomicRuntime
  { timestamp :: UTCTime
  , tmvars    :: TVars       -- TODO: this should be concurently accessible (mvar), or be a snapshot
  }

data StmRuntime = StmRuntime
  { tmvars :: TVars

  }


type STML' a = StateT AtomicRuntime IO a
type STM' a = StateT StmRuntime IO a
