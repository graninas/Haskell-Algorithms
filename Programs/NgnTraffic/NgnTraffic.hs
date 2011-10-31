{-
/****************************************************************************
** NgnTrafficParser 2.0
** Copyright (C) 2011 Granin A.S.
** Contact: Granin A.S. (graninas@gmail.com)
**
** This file is part of NgnTrafficParser 2.0.
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

module Main where

import Types
import Tools
import FileListProcess
import DataProcess

import System.Directory (getDirectoryContents, getCurrentDirectory)
import qualified Time as T (CalendarTime(..), toCalendarTime, getClockTime)
import qualified Data.ByteString.Char8 as C
import qualified System as S (getArgs) 
import Data.List as L (concat, isInfixOf, intercalate)

import Constants
import Options

helpArg1 = "-h"
helpArg2 = "--help"

copyrightMessage = [
	"",
	"NgnTrafficParser 2.0",
	"Copyright (C) 2011 Granin A.S.",
	"graninas@gmail.com",
	""]

helpMessage = [
	"Options:",
	"-f [<Field index list>] - fields to take.",
	"-ym (Year, Month) - month wich will be taken.",
	"-w - wait keypressing after done.",
	helpArg1 ++ ", " ++ helpArg2 ++ " - this help message.",
	"",
	"Previous month will be taken if no -ym option was provided.",
	"Default fields: " ++ show defaultFieldIndexes
	]


process' :: ResFilePath -> FieldIndexes -> FilePath -> IO ()
process' resFile fis targetFile = do
			fileContents <- C.readFile targetFile
			let processResult = processData fis predicates fileContents
			C.appendFile resFile processResult

process :: ResFilePath -> [FilePath] -> FieldIndexes -> IO String
process _ [] _ = return ("No files to process.\n" ++ (intercalate "\n" copyrightMessage))
process resFile fs fieldIndexes = do
	C.writeFile resFile C.empty
	mapM_ (process' resFile fieldIndexes) fs
	return "All ok."

-- | Возвращает первое число прошлого месяца.
getPrevMonthBegin :: IO T.CalendarTime
getPrevMonthBegin = do
	curCalendarTime <- (T.getClockTime >>= T.toCalendarTime)
	prevMonth1st <- return (prevMonthBegin curCalendarTime)
	T.toCalendarTime prevMonth1st

main :: IO ()
main = do
	targetYearMonthTime <- getPrevMonthBegin
	args <- S.getArgs

	let concated = concat args
	if (helpArg1 `isInfixOf` concated || helpArg1 `isInfixOf` concated)
		then putStrLn (intercalate "\n" (copyrightMessage ++ helpMessage))
		else do
			let cmdLineOptions = optionsFromArgs args
			let defaultYearMonth = toYearMonth targetYearMonthTime
			let options = compileOptions cmdLineOptions (defaultOptions defaultFieldIndexes defaultYearMonth)
			let yearMonth = getYearMonth options
			let fieldIndexes = getFieldIndexes options
			let wait = getWait options
			putStrLn ("Opts:" ++ show options)
			dirContents <- (getCurrentDirectory >>= getDirectoryContents)
			files <- return (monthFiles dirContents yearMonth)
			putStrLn (unlines files)
			res <- process (resultFileName yearMonth) files fieldIndexes
			putStrLn res
			if wait then getLine >>= putStrLn else putStrLn ""
