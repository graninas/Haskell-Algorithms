{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PhantomTypeCoerce where

import Control.Monad.Free
import Prelude hiding (read)
import Unsafe.Coerce
import Data.Typeable

newtype Measurement tag = Measurement Float
  deriving (Show, Read, Eq)

data Kelvin
data Pascal
type Temperature = Measurement Kelvin
type Pressure    = Measurement Pascal

data Procedure tag a where
    Read :: tag -> (Measurement tag -> a) -> Procedure tag a
    RunCommand :: String -> a -> Procedure tag a
  deriving (Functor)

type Script tag a = Free (Procedure tag) a

-- private function:
read :: tag -> Script () (Measurement tag)
read p = liftF (Read p id)

-- Free DSL:
readTemperature :: Script () Temperature
readTemperature = read (undefined :: Kelvin)

readPressure :: Script () Pressure
readPressure = read (undefined :: Pascal)

runCommand :: String -> Script () ()
runCommand cmd = liftF (RunCommand cmd ())

-- Unsafe, want to get rid of.
untag = unsafeCoerce

toKelvin :: Float -> Temperature
toKelvin = Measurement
fromKelvin :: Measurement Kelvin -> Float
fromKelvin (Measurement v) = v
toPascal :: Float -> Pressure
toPascal = Measurement
fromPascal :: Measurement Pascal -> Float
fromPascal (Measurement v) = v

--readT :: Script () Float
readT = do
    t <- readTemperature
    return $ fromKelvin t

--readP :: Script () Float
readP = do
    t <- readPressure
    return $ fromPascal t

-- Script to be interpreted
--script :: Script () (Float, Float, Float)
script = do
        temp1  <- readT
        press1 <- readP
        temp2  <- readT
        return (temp1, press1, temp2)

-- Interpreter
scriptInterpreter (Pure a)    = return a
scriptInterpreter (Free proc) = case proc of
    Read p next -> case cast p of
        Just (_ :: Kelvin) -> do
            print "Read temperature."
            scriptInterpreter (next $ toKelvin 100.0)
        Nothing -> error $ "Bad cast: " ++ show (typeOf p)
    RunCommand cmd next -> do
        print $ "Some command to be run: " ++ cmd
        scriptInterpreter next

