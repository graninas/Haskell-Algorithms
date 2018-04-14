module AdvGame.AdvGameRuntime where

import           Control.Monad (mapM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import           Control.Monad.Free    (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (StateT, get, put)

import           AdvGame.Lang

data AdvGameRuntime = AdvGameRuntime
  { _inventory :: Map.Map String Item
  }

type Interpreter a = StateT AdvGameRuntime IO a

interpret :: AdventureLF s -> Interpreter s
interpret (PrintS s next)  = const next <$> (lift $ putStrLn s)
interpret (Put s next)     = error "Not implemented."
interpret (Drop s next)    = error "Not implemented."
interpret (List next)      = do
  AdvGameRuntime inv <- get
  mapM (lift . putStrLn . snd) $ Map.toList inv
  pure next

run :: AdventureL s -> Interpreter (String, s)
run l = do
  result <- foldFree interpret l
  input  <- lift getLine
  pure (input, result)

initialAdvGameRuntime :: AdvGameRuntime
initialAdvGameRuntime = AdvGameRuntime Map.empty
