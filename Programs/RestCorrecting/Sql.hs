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

module Sql where

import Database.HDBC
import qualified Data.ByteString.Char8 as BS

import Types

convertSqlRow :: [SqlValue] -> RawRow
convertSqlRow = map convertSqlValue

convertSqlData :: [[SqlValue]] -> RawData
convertSqlData = map convertSqlRow

convertSqlValue :: SqlValue -> RawValue
convertSqlValue (SqlByteString s) = case reads (BS.unpack s) of
								[(x, _)] -> DoubleVal x
								[] -> DoubleVal 0.0
convertSqlValue s@(SqlInt32 _) = Int32Val (fromSql s)
convertSqlValue s@(SqlInt64 _) = Int32Val (fromSql s)