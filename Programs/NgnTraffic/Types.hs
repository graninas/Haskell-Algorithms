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

module Types where

import qualified Data.ByteString.Char8 as C

type Year = Int
type Month = Int
type YearMonth = (Year, Month)

type ResFilePath = FilePath

type FieldIndex = Int
type FieldIndexes = [FieldIndex]
type FieldDelimiter = Char
type Fields = [C.ByteString]

data Predicate =  NotInList [C.ByteString]
				| InList [C.ByteString]
type PredicateMap = [(FieldIndex, Predicate)]


data Option = FieldsToCollect FieldIndexes
			| TargetYearMonth YearMonth
			| WaitAfterDone
	deriving (Show,	Read)

type Options = [Option]

instance Eq Option where
	(FieldsToCollect x) == (FieldsToCollect y) = True
	(TargetYearMonth x) == (TargetYearMonth y) = True
	(WaitAfterDone) == (WaitAfterDone) = True
	a == b = False
