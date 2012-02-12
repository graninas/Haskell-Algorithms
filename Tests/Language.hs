module Main where

import Language.Haskell.Parser


main = do
    s <- readFile "Fact.hs"
    let parsed = parseModule s
    putStrLn . show $ parsed
    writeFile "fact_parsed.txt" (show parsed)
