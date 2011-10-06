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

module TestData where


import Types

testDataCharges :: Charges
testDataCharges = [
	(882513, 83, 146 , 0  ),
	(888914, 86, 20.14 , 0  ),
	(901151, 83, 146 , 0  ),
	(910652, 83, 160 , 0  ),
	(923097, 86, 25.2 , 0  ),
	(925555, 97, 4.5 , 0 ),
	(933432, 86, 24.84 , 0  ),
	(942629, 83, 160 , 0 ),
	(953871, 86, 25.56 , 0 ),
	(961474, 83, 160 , 0  ),
	(970190, 86, 6.12 , 0  ),
	(980051, 83, 160 , 0  ),
	(989861, 86, 8.64 , 0  ),
	(999726, 83, 160 , 0  ),
	(1007970, 86, 5.4 , 0  ),
	(1016764, 83, 160 , 0  ),
	(1025142, 93, 6 , 0  ),
	(1028077, 86, 6.48 , 0.0  ),
	(1034160, 83, 160 , 160  ),
	(1045654, 86, 9.36 , 9.36  ),
	(1051899, 83, 160 , 0  ),
	(1063557, 86, 18.36 , 0  ),
	(1065272, 83, (-252.9) , (-200)  )
	]

testDataPayments :: Payments
testDataPayments = [
 (408703, 0, 113.88 , 0 ),
 (417630, 0, 180 , 0  ),
 (425139, 0, 193 , 0  ),
 (431755, 0, 185 , 0  ),
 (439355, 0, 185 , 0  ),
 (446878, 0, 200 , 0  ),
 (461092, 0, 300 , 52.88  ),
 (468648, 0, 180 , 150  ),
 (475296, 0, 170 , 160  )
 ]