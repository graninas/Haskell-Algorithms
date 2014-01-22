module Main where

main = do
    putStrLn "Printing FizzBuzzes..."
    print fizzBuzzes

fizzBuzzes = map fizzBuzz [1..100]

fizzBuzz x | isDivided x 15  = "FizzBuzz"
                | isDivided x 5    = "Buzz"
                | isDivided x 3    = "Fizz"
                | otherwise         = show x

isDivided x n = (x `mod` n) == 0


hello :: IO ()
hello = do
    putStrLn "What is your name?"
    yourName <- getLine
    putStr "Hello, "
    putStrLn (yourName ++ "!")