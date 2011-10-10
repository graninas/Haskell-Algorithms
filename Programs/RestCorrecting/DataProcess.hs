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

module DataProcess where

import Types

nonDistributableServiceIDs = [76, 66]
nonProcessableSubscriberIDs = ["6941", "8465", "6077", "7608", "10087"]

chargeTemplate = "SELECT ID_NACH, ID_USL, SUMMA, OST_NACH FROM NACH WHERE NACH_PRZ <> 2 AND ID_USL NOT IN (74, 1) AND ID_OBSH = %s ID_OBSH NOT IN (" ++ unwords nonProcessableSubscriberIDs ++ ") ORDER BY DATA_NACH;"
paymentTemplate = "SELECT ID_OPL, ID_USL, SUMMA, OST_OPL FROM OPL WHERE OPL_PRZ <> 2 AND ID_USL NOT IN (74, 1) AND ID_OBSH = %s ID_OBSH NOT IN (" ++ unwords nonProcessableSubscriberIDs ++ ") ORDER BY DATA_OPL;"

chargeUpdateTemplate  = "UPDATE NACH SET OST_NACH = ROUND(%f, 2) WHERE ID_NACH = %d AND ID_OBSH = %s"
paymentUpdateTemplate = "UPDATE OPL SET OST_OPL = ROUND(%f, 2) WHERE ID_OPL = %d AND ID_OBSH = %s"

fromRawRow :: RawRow -> Record
fromRawRow ( Int32Val id
				  : Int32Val servID
				  : DoubleVal rSum
				  : DoubleVal rRest
				  : _) = (id, servID, rSum, rRest)

fromRawData :: RawData -> Records
fromRawData = map fromRawRow

diff :: Records -> Records -> Records
diff [] _ = []
diff _ [] = []
diff (x@(_, _, _, r1):xs) (y@(_, _, _, r2):ys) = if (x /= y && (abs (r1 - r2) > 0.001)) then x : diff xs ys else diff xs ys


sumChargeVals :: Charges -> (Double, Double)
sumChargeVals = foldr (\(_, _, chSum, chRest) (s,r) -> (s + chSum, r+chRest)) (0, 0)

sumPaymentVals :: Payments -> (Double, Double)
sumPaymentVals = foldr (\(_, _, pSum, pRest) (s,r) -> (s + pSum, r + pRest)) (0,0)

nullChargeRests :: Charges -> Charges
nullChargeRests [] = []
nullChargeRests ((a, servID, f, _):cs)  = (a, servID, f, 0.0) : nullChargeRests cs

nullPaymentRests :: Payments -> Payments
nullPaymentRests [] = []
nullPaymentRests ((a, servID, f, _):ps) = (a, servID, f, 0.0) : nullPaymentRests ps


distributeSumC :: Double -> Charges -> Charges
distributeSumC 0 _ = []
distributeSumC _ [] = []
distributeSumC s (charge@(a, e, f, _):cs) | s > f && f > 0 = (a, e, f, f) : distributeSumC (s - f) cs
										  | s > f && f < 0 = charge       : distributeSumC s cs
										  | otherwise = (a, e, f, s)      : cs

distributeSumP :: Double -> Payments -> Payments
distributeSumP 0 _ = []
distributeSumP _ [] = []
distributeSumP s (payment@(a, servID, f, _):cs) | s > f && f > 0 && not (servID `elem` nonDistributableServiceIDs) = (a, servID, f, f) : distributeSumP (s - f) cs
											| (s > f && f < 0) || servID `elem` nonDistributableServiceIDs = payment : distributeSumP s cs
											| otherwise = (a, servID, f, s) : cs

process :: (Charges, Payments) -> (Charges, Payments)
process ([], _) = ([], [])
process (_, []) = ([], [])
process (charges, payments) =
		case sumDiff > 0 of
			True ->  (reverse distibutedCharges, nulledPayments)
			False -> (nulledCharges, reverse distributedPayments)
	where
		(cs, cr) = sumChargeVals charges
		(ps, pr) = sumPaymentVals payments
		sumDiff  = (cs - ps)
		nulledCharges  = nullChargeRests charges
		nulledPayments = nullPaymentRests payments
		distibutedCharges   = distributeSumC (abs sumDiff) (reverse nulledCharges)
		distributedPayments = distributeSumP (abs sumDiff) (reverse nulledPayments)