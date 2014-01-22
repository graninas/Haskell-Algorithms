module Main where

getName :: IO String
getName = do
   putStrLn "What is your name?"
   getLine

writeMyName :: IO ()
writeMyName = do
    name <- getName
    writeFile "myName.txt" name

    