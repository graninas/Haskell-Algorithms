{-
/****************************************************************************
** NgnTrafficParser 1.0
** Copyright (C) 2011 Granin A.S.
** Contact: Granin A.S. (graninas@gmail.com)
**
** This file is part of NgnTrafficParser 1.0.
**
** GNU General Public License Usage
** This file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL3 included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/licenses/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** author (graninas@gmail.com).
**
****************************************************************************/
-}

import System (getArgs)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import Char (isDigit, toUpper, toLower)
import Text.Printf (printf)
import qualified Time as T (TimeDiff(..), CalendarTime(..), toCalendarTime, getClockTime, addToClockTime, toClockTime)
import qualified System.CPUTime as CT
import qualified Data.ByteString.Char8 as C

type Year = Int
type Month = Int
type DateRange = (Year, Month)


testAction = "Trying to replace String with Data.ByteString.Lazy.Char8 in file merging." ++ "\n"

-------- Код парсера. ----------------------------------------------------------------------------
-- Индексы полей по умолчанию.
defaultNeededFields = [6, 7, 8, 11, 13, 35, 111, 121]

isR200 :: [String] -> Bool
isR200 ("R200":_) = True
isR200 _ = False

replaceChar :: Char -> Char -> Char -> Char
--replaceChar whatC withC c = if c == whatC then withC else c
replaceChar whatC withC c | c == whatC = withC
replaceChar _ _ c = c

-- Чтобы пустые поля, обозначенные как "||", обрабатывались корректно, между ними вставляется пробел.
refieldDoubles :: String -> String
refieldDoubles [] = []
refieldDoubles ss@('|':[]) = ss
refieldDoubles ('|':'|':ss) = "| |" ++ (refieldDoubles ('|':ss))
refieldDoubles ss@(s:[]) = ss
refieldDoubles (s:ss) = s : (refieldDoubles ss)

-- Старый вариант:
--refieldDoubles (s:ss) = if (s == '|' && (head ss) == s) then (s : [' ']) ++ refielded else s : refielded
	--where refielded = refieldDoubles ss

-- Сначала символ пробела заменяется на *. Пробел может входить в состав какого-нибудь поля.
-- Потом пробел возвращается на свое место в функции processLine.
-- Это медленно, но зато корректно обрабатывается поле с датой-временем, разделенными пробелом.
replaceSymbols :: String -> String
--replaceSymbols s = map (replaceChar '|' ' ') ((map (replaceChar ' ' '*') (refieldDoubles s)))
replaceSymbols s = map (\x -> if x == '|' then ' ' else x) ((map (\x -> if x == ' ' then '*' else x) (refieldDoubles s)))

takeInterest :: [String] -> [Int] -> [String]
takeInterest _ [] = []
takeInterest ss ns = map (\x -> ss !! x) ns

interestFields :: [String] -> [Int] -> [String]
--interestFields ss takeWhat | (maximum takeWhat) < length ss = takeInterest ss takeWhat
--						   | otherwise = []
interestFields = takeInterest

makeNewLine :: [Int] -> [String] -> String
--makeNewLine fields str = map (replaceChar ' ' '|') (unwords (interestFields str fields))
makeNewLine fields str = map (\x -> if x == ' ' then '|' else x) (unwords (interestFields str fields))

-- Обработка каждой линии. Возвращает строку с нужными полями, если это R200,
-- возвращает [], если это не R200.
processLine :: [Int] -> String -> String
--processLine fields s = if isR200 sInWords then map (replaceChar '*' ' ') (makeNewLine fields sInWords) else []
processLine fields s = if isR200 sInWords then map (\x -> if x == '*' then ' ' else x) (makeNewLine fields sInWords) else []
						where sInWords = words . replaceSymbols $ s

processData :: [Int] -> String ->  [String]
processData [] _ = []
processData fields str = {-take 1000-} (filter (/= []) (map (processLine fields) (lines $ str)))

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

getFistMonthDay :: T.CalendarTime -> T.CalendarTime
getFistMonthDay ct = ct {T.ctDay = 1}

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
readFilesToMerge :: [String] -> IO [C.ByteString]
readFilesToMerge fsToMerge = mapM C.readFile fsToMerge

process dirContents dr neededFields = do
			let fsToMerge = filesToMerge dirContents dr
			let fsToMergeCountStr = show $ length fsToMerge
			let mergeLog = (newFileName dr ++ ".log")
			let dateRangeMsg = "\nDateRange: " ++ show dr
			fsContents <- readFilesToMerge fsToMerge
			let mergedContents = C.unlines fsContents
			C.writeFile (newFileName dr) mergedContents
			let processedStrings = processData neededFields (C.unpack mergedContents)
			let processedContentStr = unlines $ processedStrings
			writeFile "processed.txt" processedContentStr
			putStrLn (unlines . take 100 $ processedStrings)
			putStrLn "(Top 100 lines)"
			putStrLn dateRangeMsg
			putStrLn $ "Total files: " ++ fsToMergeCountStr
			putStrLn $ "Total lines: " ++ show (length processedStrings)
			putStrLn $ printf "See %s for file list." mergeLog
			writeFile  mergeLog (unlines fsToMerge)
			appendFile mergeLog dateRangeMsg
			appendFile mergeLog ("\nTotal files: " ++ fsToMergeCountStr)
			appendFile mergeLog ("\nTotal lines: " ++ show (length processedStrings))

main :: IO ()
main = do
	args <- getArgs
	curDir <- getCurrentDirectory
	dirContents <- getDirectoryContents curDir
	curTime <- T.getClockTime
	curCalendarTime <- T.toCalendarTime curTime
	monthAgoTime <- return $ T.addToClockTime (T.TimeDiff 0 (-1) 0 0 0 0 0) (T.toClockTime $ getFistMonthDay curCalendarTime)
	calendarMonthAgoTime <- T.toCalendarTime monthAgoTime
	let closeWhenDone = case map toUpper . take 2 . dropWhile (/= '-') . unwords $ args of
						"-W" -> False
						_ -> True
	let maybeDateRange = case dropWhile (/= '(') . takeWhile (/= ')') . unwords $ args of
						[] -> Just $ defaultDateRange calendarMonthAgoTime
						fstArgs -> readDateRange (fstArgs ++ ")")
	let neededFields = case takeWhile (/= ']') . dropWhile (/= '[') . unwords $ args of
						[] -> defaultNeededFields
						sndArgs -> case readNeededFields $ sndArgs ++ "]" of
									Just x -> x
									Nothing -> defaultNeededFields
	let func = case maybeDateRange of
				Just dr -> process dirContents dr neededFields
				Nothing -> putStrLn ("Invalid date range.")

	putStrLn "Working, please wait...\n" >> func
	if closeWhenDone then return () else getLine >> return ()
	
-- -- -- Optimization tests. -- -- --
--	time1 <- CT.getCPUTime
-- func
{-	time2 <- CT.getCPUTime
	time3 <- CT.getCPUTime
	func
	time4 <- CT.getCPUTime
	time5 <- CT.getCPUTime
	func
	time6 <- CT.getCPUTime
	let showedTime1 = show (time2 - time1) ++ "\n"
	let showedTime2 = show (time4 - time3) ++ "\n"
	let showedTime3 = show (time6 - time5) ++ "\n"
	appendFile "test.log" testAction
	appendFile "test.log" showedTime1
	appendFile "test.log" showedTime2
	appendFile "test.log" showedTime3
	processedTxt <- readFile "processed.txt"
	putStrLn . take 10000 $ processedTxt
	if null processedTxt then appendFile "test.log" " --Failed: no data." else return ()
	if closeWhenDone then return () else getLine >> return () -}


-- Some help:
-- mapM readFile fsToMerge == sequence $ map readFile fsToMerge
-- readFile :: FilePath -> IO String
-- map :: (a -> b) -> [a] -> [b]
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- sequence :: Monad m => [m a] -> m [a]

-- intercalate :: [a] -> [[a]] -> [a]
-- intercalate xs xss is equivalent to (concat (intersperse xs xss)). It inserts the list xs in between the lists in xss and concatenates the result.

-- lines :: String -> [String]
-- lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines.

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter, applied to a predicate and a list, returns the list of those elements that satisfy the predicate; i.e.,
-- filter p xs = [ x | x <- xs, p x]

-- readFile :: FilePath -> IO String
-- The readFile function reads a file and returns the contents of the file as a string. The file is read lazily, on demand, as with getContents.

-- writeFile :: FilePath -> String -> IO ()
-- The computation writeFile file str function writes the string str, to the file file.

-- getArgs :: IO [String]
-- Computation getArgs returns a list of the program's command line arguments (not including the program name).