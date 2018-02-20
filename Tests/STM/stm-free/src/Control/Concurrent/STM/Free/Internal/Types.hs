module Control.Concurrent.STM.Free.Internal.Types where

import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)
import           Control.Monad.Free
import           Control.Concurrent.MVar (MVar, takeMVar, newMVar, putMVar)
import           Control.Monad.State.Strict       (StateT, evalStateT, get,
                                                     modify, put)


type TVarId = Int

type TVars = Map.Map Int (MVar BSL.ByteString)

data StmlRuntime = StmlRuntime
  { tvars :: TVars

  }


type STM' a = StateT StmlRuntime IO a
