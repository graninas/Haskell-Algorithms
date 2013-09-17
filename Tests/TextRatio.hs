module Main where

import qualified Data.List as L

englishLetters = ['a'..'z'] ++ ['A'..'Z']


lettersCount :: String -> (Int, Int)
lettersCount text = let (res, _) = L.mapAccumL f (0,0) text
                    in res
  where
    f (a, t) x | x `elem` englishLetters = ((a + 1, t + 1), x)
    f (a, t) x | otherwise               = ((a    , t + 1), x)

    
divide l t = (fromIntegral l) / (fromIntegral t)

main = do
    --let testText = "ABC2@#%#$^#$&f"
    fileCont <- readFile "text.txt"
    let (letters, total) = lettersCount fileCont
    putStrLn $ "Letters count: " ++ show letters
    putStrLn $ "Total count: " ++ show total
    putStrLn $ "Ratio = " ++ (show $ divide letters total)
    putStrLn "Ok."