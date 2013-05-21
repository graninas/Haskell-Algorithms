module Main where

main :: IO ()
main = do
    putStrLn "Calculating FizzBuzzes..."
    let resultStrings = map fizzBuzz [1..100]

    putStrLn "Printing FizzBuzzes..."
    mapM_ putStrLn resultStrings

fizzBuzz :: Int -> String
fizzBuzz x | isDivided x 15 = "FizzBuzz"
           | isDivided x 5 = "Buzz"
           | isDivided x 3 = "Fizz"
           | otherwise = show x

isDivided x n = (x `mod` n) == 0