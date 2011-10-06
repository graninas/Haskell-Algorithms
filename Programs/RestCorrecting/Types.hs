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

module Types where


data RawValue = DoubleVal Double
				| Int32Val  Int
			--	| Int64Val  Int
	deriving (Show)

type RawRow = [RawValue]
type RawData = [RawRow]

--            (ID_RECORD, ID_USL, SUMMA, OST_REC)
type Record = (Int, Int, Double, Double)
type Records = [Record]

type Charge = Record
type Charges = Records

type Payment = Record
type Payments = Records

