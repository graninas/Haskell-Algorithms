module Main where

main :: IO ()
main = do
    putStrLn "Printing FizzBuzzes..."
    mapM_ putStrLn fizzBuzzes



fizzBuzzes :: [String]
fizzBuzzes = iterateList [1..100]
iterateList :: [Int] -> [String]
iterateList [] = []
iterateList (x : xs) = fizzBuzz x : iterateList xs


fizzBuzzes' :: [String]
fizzBuzzes' = accumulate' 1
accumulate' :: Int -> [String]
accumulate' 100 = [ fizzBuzz 100 ]
accumulate' n     = fizzBuzz n : accumulate' (n + 1)


fizzBuzz :: Int -> String
fizzBuzz x | isDivided x 15  = "FizzBuzz"
           | isDivided x 5   = "Buzz"
           | isDivided x 3   = "Fizz"
           | otherwise       = show x

fizzBuzz' :: Int -> String
fizzBuzz' x = case isDivided x 15 of
    True  -> "FizzBuzz"
    False -> case isDivided x 5 of
        True  -> "Buzz"
        False -> case isDivided x 3 of
            True  -> "Fizz"
            False -> show x

isDivided :: Int -> Int -> Bool
isDivided x n = (x `mod` n) == 0