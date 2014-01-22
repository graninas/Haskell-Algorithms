module Main where

import qualified System.Random as R

rndG1 = R.mkStdGen 10101010
rndG2 = R.mkStdGen 2424
rndG3 = R.mkStdGen 663536
rndG4 = R.mkStdGen 223636
rndG5 = R.mkStdGen 484884

ints from to = [from..to]

negativeInts from to = map negate (ints from to)

uppers = ['A'..'Z']
lowers = ['a'..'z']
letters = lowers ++ uppers

rndIndex :: RandomGen g => g -> Int -> (Int, g)
rndIndex rndGen upperBound = R.randomR (0, upperBound) rndGen

randomItem rndGen list = let
    (idx, _) = rndIndex rndGen (length list - 1)
    in list !! idx

names = (randomItem rndGen1 uppers) : 