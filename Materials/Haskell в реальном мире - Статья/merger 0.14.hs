import System (getArgs)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Char (isDigit)
import Text.Printf (printf)
import qualified Time as T

type Year = Int
type Month = Int
type DateRange = (Year, Month)

defaultDateRange :: T.CalendarTime -> DateRange
defaultDateRange (T.CalendarTime y m _ _ _ _ _ _ _ _ _ _)= (y, fromEnum m + 1)

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

merge :: [String] -> IO [String]
merge fsToMerge = mapM readFile fsToMerge

newFileName :: DateRange -> String
newFileName (y, m) = printf "%s.%02s.txt" (show y) (show m)

main :: IO ()
main = do
	args <- getArgs
	curDir <- getCurrentDirectory
	dirContents <- getDirectoryContents curDir
	curTime <- T.getClockTime
	monthAgoTime <- return $ T.addToClockTime (T.TimeDiff 0 (-1) 0 0 0 0 0) curTime
	calendarMonthAgoTime <- T.toCalendarTime monthAgoTime
	let maybeDateRange = case args of
						(a:b:_) -> readDateRange (unwords [a, b])
						_ -> Just $ defaultDateRange calendarMonthAgoTime
	case maybeDateRange of
		Just dr -> do
					let fsToMerge = filesToMerge dirContents dr
					let fsToMergeCountStr = show $ length fsToMerge
					let mergeLog = (newFileName dr ++ ".log")
					let dateRangeMsg = "DateRange: " ++ show dr
					fsContents <- merge fsToMerge
					writeFile (newFileName dr) (unlines fsContents)
					writeFile mergeLog (unlines fsToMerge ++ printf "\n%s\nTotal files: %s" dateRangeMsg fsToMergeCountStr)
					putStrLn (unlines fsContents)
					putStrLn dateRangeMsg
					--putStrLn ("Files to merge: " ++ unlines fsToMerge)
					putStrLn (printf "Count of files: %s. See %s for file list." fsToMergeCountStr mergeLog)
		Nothing -> putStrLn ("Invalid date range.")