module Main where

fizzBuzz :: Int -> String

fizzBuzz x | isDivided x 15 = "FizzBuzz"
           | isDivided x 5  = "Buzz"
           | isDivided x 3  = "Fizz"
           | otherwise      = show x


isDivided :: Int -> Int -> Bool
isDivided x n = (x `mod` n) == 0

isDivided' :: Int -> (Int -> Bool)
isDivided' x = \n -> (x `mod` n) == 0

isDivided'' :: (Int -> Int -> Bool)
isDivided'' = \x n -> (x `mod` n) == 0

fizzBuzzes :: [String]
fizzBuzzes = map fizzBuzz [1..100]


