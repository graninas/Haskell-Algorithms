module TypeClasses where

import Prelude hiding (sum)

data Point = Point Int Int

class SumProvider t where
    sum :: t -> Int -> Int -> Int

class MulProvider t where
    mul :: t -> Int -> Int -> Int

data Summator1 = S1
data Summator2 = S2

data Multiplier1 = M1 Int

instance SumProvider Summator1 where
    sum S1 x y = x + y

instance SumProvider Summator2 where
    sum S2 x y = x + y + 100

instance MulProvider Multiplier1 where
    mul (M1 coeff) x y = x * y * coeff

dotProduct :: (SumProvider s, MulProvider m) => s -> m -> Point -> Point -> Int
dotProduct s m x y = mul m (sum s 3 5) 10

main = do
    let result1 = dotProduct S1 (M1 1) (Point 0 0) (Point 0 0)
    let result2 = dotProduct S2 (M1 2) (Point 0 0) (Point 0 0)
    
    print result1   -- 80
    print result2   -- 2060
    
    
    