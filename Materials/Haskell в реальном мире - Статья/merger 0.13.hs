import System (getArgs)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Char (isDigit)

type Year = Int
type Month = Int
type DateRange = (Year, Month)

defaultDateRange :: DateRange
defaultDateRange = (2011, 4)

readDateRange :: Read a => String -> Maybe a
readDateRange str = case reads str of
						[(x, _)] -> Just x
						_ -> Nothing

isFileForMerge :: String -> DateRange -> Bool
isFileForMerge s (y, m) | (25 == length s) && (all isDigit (take 21 s)) =
										let
											ry = read ((take 4 . drop 5) s)
											rm = read ((take 2 . drop 9) s)
										in ry == y && rm == m
isFileForMerge _ _ | otherwise = False

isTxt :: String -> Bool
isTxt s = (drop ((length s) - 3) s) == "txt"

filesToMerge :: [String] -> DateRange -> [String]
filesToMerge ss dr = filter (\x -> isTxt x && isFileForMerge x dr) ss

						
main :: IO ()
main = do
	args <- getArgs
	curDir <- getCurrentDirectory
	dirContents <- getDirectoryContents curDir
	let maybeDateRange = case args of
						(a:b:_) -> readDateRange (unwords [a, b])
						_ -> Just defaultDateRange
	case maybeDateRange of
		Just dr -> do
					fsToMerge = filesToMerge dirContents dr
					fsContents = merge fsToMerge
					putStrLn ("Files to merge: " ++ unlines fsToMerge)
		Nothing -> putStrLn ("Invalid date range.")