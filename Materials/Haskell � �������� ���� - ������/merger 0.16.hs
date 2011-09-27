import System (getArgs)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Char (isDigit, toUpper, toLower)
import Text.Printf (printf)
import qualified Time as T

type Year = Int
type Month = Int
type DateRange = (Year, Month)


data Command = Fields
	deriving (Eq, Show, Read)


-------- Код парсера. ----------------------------------------------------------------------------
-- Индексы полей по умолчанию.
defaultNeededFields = [6, 7, 8, 11, 13, 35, 111, 121]

isR200 :: [String] -> Bool
isR200 [] = False
isR200 s = (head s) == "R200"

replaceChar :: Char -> Char -> Char -> Char
replaceChar whatC withC c = if c == whatC then withC else c

-- Чтобы пустые поля, обозначенные как "||", обрабатывались корректно, между ними вставляется пробел.
refieldDoubles :: String -> String
refieldDoubles [] = []
refieldDoubles ('|':[]) = "|"
refieldDoubles ('|':'|':ss) = "| |" ++ (refieldDoubles ('|':ss))
refieldDoubles (s:[]) = [s]
refieldDoubles (s:ss) = s : (refieldDoubles ss)
-- Старый вариант:
--refieldDoubles (s:ss) = if (s == '|' && (head ss) == s) then (s : [' ']) ++ refielded else s : refielded
	--where refielded = refieldDoubles ss
 
-- Сначала символ пробела заменяется на *. Пробел может входить в состав какого-нибудь поля.
-- Потом пробел возвращается на свое место в функции processLine.
-- Это медленно, но зато корректно обрабатывается поле с датой-временем, разделенными пробелом.
replaceSymbols :: String -> String
replaceSymbols s = map (replaceChar '|' ' ') ((map (replaceChar ' ' '*') (refieldDoubles s)))

takeInterest :: [String] -> [Int] -> [String]
takeInterest _ [] = []
takeInterest ss (n:ns) = [ss !! n] ++ takeInterest ss ns

interestFields :: [String] -> [Int] -> [String]
interestFields ss takeWhat | (maximum takeWhat) < length ss = takeInterest ss takeWhat
						   | otherwise = []
						   
makeNewLine :: [Int] -> [String] -> String
makeNewLine fields str = map (replaceChar ' ' '|') (unwords (interestFields str fields))

-- Обработка каждой линии. Возвращает строку с нужными полями, если это R200,
-- возвращает [], если это не R200.
processLine :: [Int] -> String -> String
processLine fields s = if isR200 sInWords then map (replaceChar '*' ' ') (makeNewLine fields sInWords) else []
						where sInWords = words . replaceSymbols $ s

processData :: [Int] -> String ->  [String]
processData [] _ = []
processData fields str = {-take 1000-} (filter (\a -> a /= []) (map (processLine fields) (lines $ str)))

readNeededFields :: Read a => String -> Maybe a
readNeededFields str = case reads str of
							[(x, "")] -> Just x
							_ -> Nothing

capitalize :: String -> String
capitalize word = (toUpper . head) word : (map toLower (tail word))

------ Код мержера. ----------------------------------------------------------
-- | Возвращает DateRange по умолчанию для указанного календарного времени.
defaultDateRange :: T.CalendarTime -> DateRange
defaultDateRange (T.CalendarTime y m _ _ _ _ _ _ _ _ _ _)= (y, fromEnum m + 1)

-- | Пытается распарсить строку с типом DateRange.
readDateRange :: Read a => String -> Maybe a
readDateRange str = case reads str of
						[(x, _)] -> Just x
						_ -> Nothing

-- | Проверяет, нужно ли мержить файл с таким именем.
isFileForMerge :: String -> DateRange -> Bool
isFileForMerge s (y, m) | (25 == length s) && (all isDigit (take 21 s)) =
										let
											ry = read ((take 4 . drop 5) s)
											rm = read ((take 2 . drop 9) s)
										in ry == y && rm == m
isFileForMerge _ _ | otherwise = False

-- | Проверяет, чтобы файл был txt.
isTxt :: String -> Bool
isTxt s = (drop ((length s) - 3) s) == "txt"

-- | Возвращает список имен файлов, которые нужно смержить за данный период.
filesToMerge :: [String] -> DateRange -> [String]
filesToMerge ss dr = filter (\x -> isTxt x && isFileForMerge x dr) ss

-- | Возвращает формат файла вывода.
newFileName :: DateRange -> String
newFileName (y, m) = printf "%s.%02s.txt" (show y) (show m)

-- | Получает список файлов и читает их содержимое. Возвращает список содержимого файлов.
readFilesToMerge :: [String] -> IO [String]
readFilesToMerge fsToMerge = mapM readFile fsToMerge

main :: IO ()
main = do
	args <- getArgs
	curDir <- getCurrentDirectory
	dirContents <- getDirectoryContents curDir
	curTime <- T.getClockTime
	monthAgoTime <- return $ T.addToClockTime (T.TimeDiff 0 (-1) 0 0 0 0 0) curTime
	calendarMonthAgoTime <- T.toCalendarTime monthAgoTime
	let closeWhenDone = case map toUpper . take 2 . dropWhile (/= '-') . unwords $ args of
						"-W" -> False
						_ -> True
	let maybeDateRange = case dropWhile (/= '(') . takeWhile (/= ')') . unwords $ args of
						[] -> Just $ defaultDateRange calendarMonthAgoTime
						fstArgs -> readDateRange (fstArgs ++ ")")
	let nedeedFields = case takeWhile (/= ']') . dropWhile (/= '[') . unwords $ args of
						[] -> defaultNeededFields
						sndArgs -> case readNeededFields $ sndArgs ++ "]" of
									Just x -> x
									Nothing -> defaultNeededFields

	putStrLn "Working, please wait...\n"
	case maybeDateRange of
		Just dr -> do
					let fsToMerge = filesToMerge dirContents dr
					let fsToMergeCountStr = show $ length fsToMerge
					let mergeLog = (newFileName dr ++ ".log")
					let dateRangeMsg = "DateRange: " ++ show dr
					fsContents <- readFilesToMerge fsToMerge
					let mergedContents = unlines fsContents
					writeFile (newFileName dr) mergedContents
					let processedContentStr = unlines $ processData nedeedFields mergedContents
					writeFile "processed.txt" processedContentStr
					putStrLn processedContentStr
					writeFile mergeLog (unlines fsToMerge ++ printf "\n%s\nTotal files: %s" dateRangeMsg fsToMergeCountStr)
					putStrLn dateRangeMsg
					putStrLn (printf "Count of files: %s. See %s for file list." fsToMergeCountStr mergeLog)
					if closeWhenDone then return () else getLine >> return ()
		Nothing -> putStrLn ("Invalid date range.") >> getLine >> return ()
