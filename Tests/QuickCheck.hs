module Main where

import qualified Data.Char as C
import qualified Data.List as L
import Test.QuickCheck

getList = find 5 where
     find 0 = return []
     find n = do
       ch <- getChar
       if ch `elem` ['a'..'e'] then do
             tl <- find (n-1)
             return (ch : tl) else
           find n
 


upperCase str = map C.toUpper str