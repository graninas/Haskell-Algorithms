{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module FreeIODSL where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Map as M

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show, Eq)
  
data Property a = Get String (Value -> a)
                | Set String Value a
  deriving (Functor)

type ObjectIOF a = FreeT Property IO a

setP prop val = liftF (Set prop val ())
getP prop = liftF (Get prop id)

type Object = M.Map String Value
type Interpreter a = StateT Object IO a

interpretObject :: ObjectIOF a -> Interpreter a
interpretObject obj = do
    x <- liftIO $ runFreeT obj
    interpretObject' x

interpretObject' (Pure a) = return a
interpretObject' (Free a) = interpretProperty a

--interpretProperty :: Property a -> Interpreter a
interpretProperty (Get prop next) = do
    obj <- get
    case M.lookup prop obj of
         Just v -> interpretObject (next v)
         Nothing -> error $ show ("No property", prop)
interpretProperty (Set prop val next) = do
    obj <- get
    put $ M.insert prop val obj
    interpretObject next

value1 = StringValue "Prop1"

script :: ObjectIOF Value
script = do
    setP "P1" value1
    liftIO $ putStrLn "Ho-ho!"
    getP "P1"
     
test = do
    (v, obj) <- runStateT (interpretObject script) M.empty
    print $ v == value1
    print $ obj == (M.fromList [("P1", value1)])