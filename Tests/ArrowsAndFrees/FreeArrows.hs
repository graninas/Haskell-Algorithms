{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module FreeArrows where

import Prelude hiding (read, id, (.))
import Control.Monad.Free
import Control.Category
import Control.Arrow
import Control.Applicative
import Data.Monoid


-- http://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows

data FreeA eff a b where
    Arr :: (a -> b) -> FreeA eff a b
    Eff :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a₁ b₁ -> FreeA eff a₂ b₂ -> FreeA eff (a₁, a₂) (b₁, b₂)

effect :: eff a b -> FreeA eff a b
effect = Eff

instance Category (FreeA eff) where
    id = Arr id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Arr
    first f = Par f id
    second f = Par id f
    (***) = Par
   
-- http://stackoverflow.com/questions/34837008/arrows-free-monads-and-io    
data IO_Eff a b where
    PutStr :: IO_Eff String ()
    GetLine :: IO_Eff () String

f :: forall a b . IO_Eff a b -> Kleisli IO a b
f PutStr = Kleisli putStr
f GetLine = Kleisli (const getLine)

type IO_EffArr a b = FreeA IO_Eff a b

putStrA :: IO_EffArr String ()
putStrA = Eff PutStr

getLineA :: IO_EffArr () String
getLineA = Eff GetLine

-- How this should work??
{-
runArr1 :: FreeA eff b c -> b -> c
runArr1 (Arr f)     b = f b
runArr1 (Eff eff)   b = eff b
runArr1 (Seq a1 a2) b = do
    v1 <- runArr1 a1 b
    c <- runArr1 a2 v1
    return c
runArr1 (Par a1 a2) b = undefined
    
runFreeArr interpret ar v = do
    let p = runArr1 ar v
    c <- interpret p
    return c
    -}


myArr = proc _ -> do
   putStrA -< "ABC"
   line <- getLineA -< ()
   returnA -< line

    


--data Free f a = Pure a
--              | Free (f (Free f a))
{-
    internalA :: FreeA eff () ()
    internalA = PureA id
    
          (FreeA eff () ())
    FreeA internalA         a b


data FreeA eff a b = PureA (a -> b)
                   | FreeA (eff (FreeA eff a b) b)

instance Category (FreeA eff) where
    id = PureA id
    PureA g . PureA f = PureA (g . f)
    PureA f . FreeA eff a b = FreeA 


printP :: String -> FreeA Process a ()
printP s = FreeA (PrintP s (Arr id))

data Procedure a
    = Test1 String a
    | Test2 (String -> a)
  deriving (Functor)

type Script a = Free Procedure a

data Process a b = ValueP (String, String) (b, Process a b)
                 | ScriptP (Script b) (b, Process a b)
                 | PrintP String (a -> b)

type Scheme a b = FreeA Process a b
    -}


{-
valueOverTime :: Scheme
valueOverTime = proc (v, t) -> do
    
                
                
  -}              
                
                
                
                
                
                
                
                
                
                
                
