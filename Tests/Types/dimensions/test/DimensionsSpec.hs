module DimensionsSpec where

import Test.Hspec

import Dimensions

data Meter
data Kilogram
data Second

type Length   = BaseQuantity Meter
type Mass     = BaseQuantity Kilogram
type Time     = BaseQuantity Second
type Area     = Square Length
type Velocity = Quotient Length Time

tableWidth :: Length
tableWidth = construct 1.5

tableHeight :: Length
tableHeight = construct 2.5

tableArea :: Area
tableArea = tableWidth .*. tableHeight

velocity :: Velocity
velocity = construct 5.0

travelTime :: Time
travelTime = construct 60.0

distance :: Length
distance = velocity .*. travelTime

spec = describe "Test." $ do
  it "test 1" $ destruct tableArea `shouldBe` 3.75
  -- it "test 2" $ destruct distance `shouldBe` 300.0
