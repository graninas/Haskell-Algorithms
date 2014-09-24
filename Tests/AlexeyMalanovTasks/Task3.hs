module Main where

import Data.List (sort, intersect)

sums1 = [6,7,13,13,19,20]
sums2 = [2,4,9,9,14,16]



bounds1 = [(-10)..30]
bounds2 = [(-30)..30]
bounds3 = [(-30.5), (-30.0)..30]

evalSums a b c d = [ a+b, a+c, a+d, b+c, b+d, c+d ]
checkSums a b c d sums = evalSums a b c d `intersect` sums == sums

solve sums bounds = [(a,b,c,d) | a <- bounds, b <- bounds, c <- bounds, d <- bounds
                               , checkSums a b c d sums ]

-- solve2 sums bounds = [(a,b) | a <- bounds, b <- map (-a) (take 3 sums) ] -- breaking typechecker!

slice x i sums = x - (sums !! i)

solve2 sums bounds = [(d,c,b,a) | a <- bounds
                                , b <- [slice a 0 sums]
                                , c <- [slice a 1 sums, slice b 3 sums]
                                , d <- [slice a 2 sums, slice b 4 sums, slice c 5 sums]
                                , checkSums d c b a sums ]
