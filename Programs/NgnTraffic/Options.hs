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

module Options where

import Types
import qualified Data.List as L ((\\))

optionsFromArgs :: [String] -> Options
optionsFromArgs [] = []
optionsFromArgs ("-f":fs) = case (reads . unwords) fs of
							[(idxList, rest)] -> (FieldsToCollect idxList) : ( (optionsFromArgs . words) rest)
							_ -> optionsFromArgs fs

optionsFromArgs ("-ym":fs) = case (reads . unwords) fs of
							[(ym, rest)] -> (TargetYearMonth ym) : ( (optionsFromArgs . words) rest)
							_ -> optionsFromArgs fs

optionsFromArgs ("-w":fs) = WaitAfterDone : optionsFromArgs fs
optionsFromArgs (arg:args) = optionsFromArgs args

defaultOptions :: FieldIndexes -> YearMonth -> Options
defaultOptions fis ym = [FieldsToCollect fis, TargetYearMonth ym]

compileOptions :: Options -> Options -> Options
compileOptions opts defOpts = opts ++ (defOpts L.\\ opts)

getYearMonth :: Options -> YearMonth
getYearMonth [] = undefined
getYearMonth ((TargetYearMonth x):_) = x
getYearMonth (_:os) = getYearMonth os

getFieldIndexes :: Options -> FieldIndexes
getFieldIndexes [] = undefined
getFieldIndexes ((FieldsToCollect x):_) = x
getFieldIndexes (_:os) = getFieldIndexes os

getWait :: Options -> Bool
getWait = not . null . filter (== WaitAfterDone)