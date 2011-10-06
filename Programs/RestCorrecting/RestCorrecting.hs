{-
/****************************************************************************
** RestCorrecting 1.0
** Copyright (C) 2011 Granin A.S.
** Contact: Granin A.S. (graninas@gmail.com)
**
** This file is part of RestCorrecting 1.0.
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

import Database.HDBC
import Database.HDBC.ODBC
import qualified Text.Printf as PF
import qualified Data.List as L

import TestData
import DataProcess
import Sql
import Types

testResult = "testResult.txt"
dbDataTest = "dbDataTest.txt"
keysFile = "id_obsh.txt"
updateScripts = "updateScripts.sql"

titleMessage = [
	"\nRestCorrecting 1.0",
	"Copyright (C) 2011 Granin A.S.",
	"graninas@gmail.com\n"]

helpMessage = ["Put the subscriber keys into " ++ keysFile ++ " file."]

generateUpdateScript :: String -> String -> Record -> String
generateUpdateScript templ key (recID, _, _, recRest) = "\n" ++ (PF.printf templ recRest recID key)

processData :: [[SqlValue]] -> [[SqlValue]] -> String -> IO ()
processData chargeVals paymentVals key = do
		appendFile dbDataTest ("\n\nSubscriber: " ++  key)
		appendFile dbDataTest "\n_____________________________________________________________________________________________"
		appendFile dbDataTest "\nRaw charge data:\n"
		appendFile dbDataTest . unlines $ (map show chargeVals)
		appendFile dbDataTest "\nRaw payment data:\n"
		appendFile dbDataTest . unlines $ (map show paymentVals)
		appendFile dbDataTest "\n_____________________________________________________________________________________________"

		let convertedCharges = convertSqlData chargeVals
		let convertedPayments = convertSqlData paymentVals
		let rawCharges = fromRawData convertedCharges
		let rawPayments = fromRawData convertedPayments
		let (c, p) = process (rawCharges, rawPayments)
		let (strC, strP) = (map show c, map show p)
		let chargeDiffs = diff c rawCharges
		let paymentDiffs = diff p rawPayments

		appendFile testResult ("\n\nSubscriber: " ++  key)
		appendFile testResult "\n_____________________________________________________________________________________________"
		appendFile testResult "\nProcesed charges:\n"
		appendFile testResult . unlines $ strC
		appendFile testResult "\nProcessed payments:\n"
		appendFile testResult . unlines $ strP
		appendFile testResult "\nCharges to update:\n"
		appendFile testResult . unlines $ (map show chargeDiffs)
		appendFile testResult "\nPayments to update:\n"
		appendFile testResult . unlines $ (map show paymentDiffs)
		appendFile testResult "\n_____________________________________________________________________________________________"

		mapM_ (appendFile updateScripts . generateUpdateScript chargeUpdateTemplate key)  chargeDiffs
		mapM_ (appendFile updateScripts . generateUpdateScript paymentUpdateTemplate key) paymentDiffs



processKey :: IConnection conn => conn -> String -> IO ()
processKey _ [] = return ()
processKey conn key = do
		chargeVals <- quickQuery conn (PF.printf "SELECT n.ID_NACH, n.ID_USL, n.SUMMA, n.OST_NACH, n.NACH_PRZ FROM NACH  n LEFT JOIN TEH_GLAV tg ON tg.ID_TEH = n.ID_TEH WHERE n.NACH_PRZ <> 2 AND n.ID_USL <> 74 AND n.ID_OBSH = %s AND tg.TEH_PRZ <> 2 ORDER BY DATA_NACH;" key) []
		putStrLn $ show chargeVals
		paymentVals <- quickQuery conn (PF.printf "SELECT ID_OPL, ID_USL, SUMMA, OST_OPL, OPL_PRZ FROM OPL WHERE OPL_PRZ <> 2 AND ID_USL <> 74 AND ID_OBSH = %s ORDER BY DATA_OPL;" key) []
		putStrLn $ show paymentVals
		processData chargeVals paymentVals key


main = do
		let connectionString =  "Driver={SQL Server};Server=192.168.4.14;Database=LTC_2011;Trusted_Connection=yes;"
		let ioconn = connectODBC connectionString
		conn <- ioconn

		writeFile dbDataTest ""
		writeFile testResult ""
		writeFile updateScripts ""

		appendFile keysFile ""
		keysS <- readFile keysFile
		
		let keys = words keysS
		if (null keys) 
			then putStrLn (L.intercalate "\n" (titleMessage ++ helpMessage))
			else do
				mapM_ (processKey conn) keys
				putStrLn "Done."
				putStrLn (L.intercalate "\n" titleMessage)