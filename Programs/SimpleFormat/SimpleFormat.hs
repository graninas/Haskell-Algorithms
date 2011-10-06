module Main where

import Process

italic = ["__", "<i>", "</i>"]
blue   = ["@@", "<font color=blue>", "</font>"]
bold   = ["**", "<b>", "</b>"]

formatters :: [[String]]
formatters = [italic, blue, bold]

format :: [String] -> String -> String
format (border : openTag : closeTag : _) s = process s border (openTag, closeTag) True


main :: IO ()
main = do
	fContents <- readFile "text.txt"

	let result = foldr format fContents formatters

	writeFile "p_text.txt" result
	writeFile "p_text.html" result
	putStrLn "Ok."