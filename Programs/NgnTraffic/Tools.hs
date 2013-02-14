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

module Tools where

import qualified Time as T (TimeDiff(..), CalendarTime(..), ClockTime(..), addToClockTime, toClockTime)
import Char (toUpper, toLower)
import Types

firstMonthDay :: T.CalendarTime -> T.CalendarTime
firstMonthDay ct = ct {T.ctDay = 1}

prevMonthBegin :: T.CalendarTime -> T.ClockTime
prevMonthBegin curMonth = T.addToClockTime (T.TimeDiff 0 (-1) 0 0 0 0 0) (T.toClockTime $ firstMonthDay curMonth)

toYearMonth :: T.CalendarTime -> YearMonth
toYearMonth (T.CalendarTime y m _ _ _ _ _ _ _ _ _ _)= (y, fromEnum m + 1)

capitalize :: String -> String
capitalize [] = []
capitalize (l:ls) = toUpper l : map toLower ls

