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

module Constants where

import Types
import Data.List (sort)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as C

fieldDelimiter = '|'
bsFieldDelimiter = C.singleton '|'

-- | Возвращает формат файла вывода.
resultFileName :: YearMonth -> FilePath
resultFileName (y, m) = printf "%s.%02s.txt" (show y) (show m)

defaultFieldIndexes :: FieldIndexes
defaultFieldIndexes = sort [7, 8, 9, 12, 14, 36, 112, 122]

predicates :: PredicateMap
predicates = [--(1,  InList [C.pack "R200"]),
			  (7,  NotInList (map C.pack ["3022", "3012"])),
			  --(8,  Like   [C.pack "44", C.pack "45"]),
			  --(9,  LengthLess 7),
			  (36, NotInList (map C.pack ["1800", "3600", "5400", "7200", "9000", "10800", "12600", "14400", "16200", "18000", "19800", "21600", "23400"]) )]

maxPredicateField = foldr (\(idx, _) y -> if idx > y then idx else y) 0 predicates

