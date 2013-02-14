import Data.List

replaceChars :: Char -> Char -> Char -> Char
replaceChars whatC withC c = if c == whatC then withC else c

interestFields :: [String] -> [Int] -> [String]
interestFields s takeWhat = undefined

isR200 :: [String] -> Bool
isR200 s = (head s) == "R200"

processLine :: String -> String
processLine s = if isR200 sInWords then unwords (interestFields sInWords [1,2,3] ) else []
				where sInWords = words ( map (replaceChars '|' ' ') s )

processString :: String -> [String]
processString s = map processLine (lines $ s)

main :: IO ()
main = do
	str <- readFile "merged.txt"
	putStrLn (intercalate "\r\n" (processString $ str))
