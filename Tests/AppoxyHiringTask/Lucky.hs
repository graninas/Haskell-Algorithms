module Main where

import Data.List as L

withEqualConsequtives :: String -> Bool
withEqualConsequtives s = any (\subS -> length subS > 1) . group $ s

composeLucky :: String -> [String]
composeLucky = L.nub . filter (not . withEqualConsequtives) . L.permutations

howManyLuckies :: String -> Either String Int
howManyLuckies s = if l >= 1 && l <= 10
          then Right $ length $ composeLucky s
          else Left "There are bounds: length should be in range [1,10]"
  where
    l = length s

main = do
    s <- readFile "lucky.txt"
    print "Luckies count:"
    print $ howManyLuckies $ init s
