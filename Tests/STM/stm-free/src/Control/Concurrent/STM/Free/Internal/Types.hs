module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map                   as Map
import           GHC.Generics               (Generic)


type TVarId = Int

type TVars = Map.Map Int (MVar BSL.ByteString)

data StmlRuntime = StmlRuntime
  { tmvars :: TVars

  }


type STM' a = StateT StmlRuntime IO a
