{-
By Denis Krjuchkov
http://stackoverflow.com/questions/2354707/in-haskell-is-there-num-a-infinity-a
-}

module Infinitable where

data Infinitable a = NegativeInfinity | Regular a | PositiveInfinity
	deriving (Eq, Show)

instance Ord a => Ord (Infinitable a) where
    compare NegativeInfinity NegativeInfinity = EQ
    compare PositiveInfinity PositiveInfinity = EQ
    compare NegativeInfinity _ = LT
    compare PositiveInfinity _ = GT
    compare _ PositiveInfinity = LT
    compare _ NegativeInfinity = GT
    compare (Regular x) (Regular y) = compare x y

main =
    let five = Regular 5
        pinf = PositiveInfinity::Infinitable Integer
        ninf = NegativeInfinity::Infinitable Integer
        results = [(pinf > five), (ninf < pinf), (five > ninf)]
    in
        do putStrLn (show results)
