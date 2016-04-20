{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
module YonedaLemma where

data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String           
  deriving (Show, Read, Eq)

data Pascal
data Kelvin


newtype Measurment a = Measurment Value
  deriving (Show, Read, Eq)

data Parameter tag = Parameter String (Measurment tag)
  deriving (Show, Read, Eq)

-- Yoneda Lemma:
-- if 'f' is a functor, then there is isomorphism:
-- (forall b. (a -> b) -> f b) ~ f a

-- f b = ModelDef (Parameter Kelvin -> a)
-- 
  
data ModelDefGadt a tag where
    TemperatureParameter :: String -> (Parameter Kelvin -> a) -> ModelDefGadt a Kelvin
    PressureParameter    :: String -> (Parameter Pascal -> a) -> ModelDefGadt a Pascal

data ModelDefGadt2 a tag where
    TemperatureParameter1 :: String -> (Kelvin -> tag) -> (Parameter Kelvin -> a) -> ModelDefGadt2 a tag
    PressureParameter1    :: String -> (Pascal -> tag) -> (Parameter Pascal -> a) -> ModelDefGadt2 a tag
    
data ModelDef a tag =
    forall t. Param String (t -> tag) (Parameter tag -> a)


    
    
boolValue :: Bool -> Value
boolValue = BoolValue
stringValue :: String -> Value
stringValue = StringValue
intValue :: Int -> Value
intValue = IntValue
floatValue :: Float -> Value
floatValue = FloatValue
    
    

toKelvin :: Float -> Measurment Kelvin
toKelvin v = Measurment (floatValue v)
fromKelvin :: Measurment Kelvin -> Float
fromKelvin (Measurment (FloatValue v)) = v
fromPascal :: Measurment Pascal -> Float
fromPascal (Measurment (FloatValue v)) = v

temperature :: Measurment Kelvin
temperature = toKelvin 0.0