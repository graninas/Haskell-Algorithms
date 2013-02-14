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

module FileListProcess where

import Types
import Char (isDigit)

-- | Проверяет, нужно ли мержить файл с таким именем.
isFileForMerge :: FilePath -> YearMonth -> Bool
isFileForMerge s (y, m) | (25 == length s) && (all isDigit (take 21 s)) =
										let
											ry = read ((take 4 . drop 5) s) -- Считываются цифры с годом и преобразуются к числу
											rm = read ((take 2 . drop 9) s) -- Считываются цифры с месяцем и преобразуются к числу
										in ry == y && rm == m
isFileForMerge _ _ | otherwise = False

-- | Проверяет, чтобы файл был txt.
isTxt :: FilePath -> Bool
isTxt s = (drop ((length s) - 3) s) == "txt"

-- | Возвращает список имен файлов, которые нужно смержить за данный период.
monthFiles :: [FilePath] -> YearMonth -> [FilePath]
monthFiles fileNames ym = filter (\x -> isTxt x && isFileForMerge x ym) fileNames