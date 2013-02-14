module Main where

import Language.Haskell.Parser
import System

main = do
    args <- getArgs
    case args of
        (a:_) -> do
            s <- readFile a
            let parsed = parseModule s
            putStrLn . show $ parsed
            writeFile (a ++ ".parsed.txt") (show parsed)
        _ -> putStrLn "Please, select a file."
