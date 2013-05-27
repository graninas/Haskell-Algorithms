module Main where

import System.Random
import Control.Monad (when)

rollDice :: (Int, Int) -> IO Int
rollDice (l, r) = randomRIO (l, r)

main = do
    putStrLn "Let dice you!"
    d20 <- rollDice (1, 20)
    putStrLn $ "Value: " ++ show d20
    case d20 >= 17 of
        True  -> putStrLn "LUCKY!!"
        False -> putStrLn "No..."

getName :: IO ()
getName = do
   putStrLn "What is your name?"
   yourName <- getLine
   putStr "Hello, "
   putStrLn (yourName ++ "!")

getName' = 
    putStrLn "What is your name?"
    >>= \_ -> getLine
    >>= \yourName -> putStr "Hello, "
    >>= \_ -> putStrLn (yourName ++ "!")


getName'' = 
    putStrLn "What is your name?"
    >> getLine
    >>= \yourName -> putStr "Hello, "
    >> putStrLn (yourName ++ "!")

