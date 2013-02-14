
import Data.Char

l1 = [1, 1, 2, 3, 5]

mul x n = x * n
pow x n = x ^ n

doubleList    = map (*2)            l1
doubleList'   = map (\x -> x * 2)   l1
doubleList''  = map (mul 2)         l1
doubleList''' = map (\x -> mul 2 x) l1

squareList    = map (^2)            l1
squareList'   = map (\x -> x * x)   l1
squareList''  = map (\x -> pow x 2) l1

powerOfTwo    = map (2^)            l1
powerOfTwo'   = map (\x -> 2 ^ x)   l1
powerOfTwo''  = map (pow 2)         l1
powerOfTwo''' = map (\x -> pow 2 x) l1


upperString [] = []
upperString (s:ss) = toUpper s : upperString ss

upperString'  ss   = map toUpper ss

upperString'' ss   = map toUpper