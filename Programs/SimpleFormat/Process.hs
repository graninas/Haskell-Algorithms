module Process where

import Data.List (isPrefixOf)

type BorderElement = String
type OpenTag = String
type CloseTag = String

process :: String -> BorderElement -> (OpenTag, CloseTag) -> Bool -> String
process [] _ _ _ = []
process xs el tags@(openT, _)  True  | el `isPrefixOf` xs = openT  ++ process (drop (length el) xs ) el tags False
process xs el tags@(_, closeT) False | el `isPrefixOf` xs = closeT ++ process (drop (length el) xs ) el tags True
process (x:xs) el tags b = x : process xs el tags b