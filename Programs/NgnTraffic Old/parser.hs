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

import Data.List
import System
import Char (toUpper, toLower)

data Command = Fields
	deriving (Eq, Show, Read)

-- Жестко закодированные индексы полей.
neededFields = [6, 7, 8, 11, 13, 35, 111, 121]

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

processString :: [Int] -> String->  [String]
processString [] _ = []
processString fields str = {-take 1000-} (filter (\a -> a /= []) (map (processLine fields) (lines $ str)))


parseNeededFields :: [String] -> [Int]
parseNeededFields strs = read (unwords strs) :: [Int]

-- <Участок кода из Advgame.hs>
capitalize :: String -> String
capitalize word = (toUpper . head) word : (map toLower (tail word))

maybeReadCommand :: Read a => String -> Maybe a
maybeReadCommand s = case reads s of
					[(x,"")] -> Just x
					_ -> Nothing

parseCommand :: String -> Maybe Command
parseCommand input = do
            r <- maybeReadCommand (caps input) 
            return r
            where caps = unwords . map capitalize . words
-- </Участок кода из Advgame.hs>

main :: IO ()
main = do
	str <- readFile "merged.txt"
	args <- getArgs
	command <- if null args then return Nothing else return (parseCommand . head $ args)
	func <- case command of
			Just Fields -> do
				fields <- return (parseNeededFields $ tail $ args)
				return (intercalate "\n" (processString fields str))
			
			Nothing -> do
				let processedStr = intercalate "\n" (processString neededFields str)
				return processedStr
	putStrLn func
	writeFile "processed.txt" func

-- Help:   
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
