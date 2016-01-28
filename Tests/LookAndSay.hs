module Main where

import Data.List as L
import Data.List.Split

las :: String -> String
las = concatMap (concat.map say.group) . splitOn ", "
    where
        say ss = show (length ss) ++ [head ss]

lookAndSay :: [String]
lookAndSay = iterate las "1"