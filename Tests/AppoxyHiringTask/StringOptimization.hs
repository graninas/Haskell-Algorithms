module Main where

import Data.Maybe (fromJust)

difference x y | length x == length y = Just $ difference' 0 x y
               | otherwise = Nothing
  where
    difference' d [] [] = d
    difference' d (x:xs) (y:ys) | x /= y = difference' (d + 1) xs ys
                                | otherwise = difference' d xs ys


findMinSubstringPos aLength minP curP dif [] _  = minP
findMinSubstringPos aLength minP curP dif _ []       = minP
findMinSubstringPos aLength minP curP dif a b@(_:bs) = case difference a (take aLength b) of
    Just d -> if d < dif
              then findMinSubstringPos aLength curP (curP + 1) d a bs
              else findMinSubstringPos aLength minP (curP + 1) dif a bs
    Nothing -> minP

fillFromB minIdx a b = take (minIdx - 1) b
                     ++ a
                     ++ drop (minIdx - 1 + length a) b

optimizeStrings a b | length a == length b = difference a b
                    | otherwise = let
    aLength = length a
    minimumIndex = findMinSubstringPos aLength 1 1 (maxBound :: Int) a b
    minString = fillFromB minimumIndex a b
    in difference minString b

minimize :: String -> String -> Either String Int
minimize a b | length a > length b = Left "Length of A should be less or equal than length of B."
             | otherwise = Right $ fromJust $ optimizeStrings a b


main = do
    a <- readFile "a.txt"
    b <- readFile "b.txt"
    putStrLn "Minimization difference:"
    print $ minimize a b
