
import qualified Time as CT
import qualified Text.Printf as T

data Scale = Minute | Hour | Day | Week | Month

type Price = Float
type OpenPrice = Price
type ClosePrice = Price
type MinPrice = Price
type MaxPrice = Price
type Amount = Int




collect' _ res results [] = res : results

collect' timeDiff res@(curStartTime, openPrice, closePrice, minPrice, maxPrice, totalAmount) results ((ctime, price, amount) : xs)
	| ctime < nextStartTime = 
		let lastResult = (curStartTime, openPrice, price, min price minPrice, max price maxPrice, amount + totalAmount)
		in collect' timeDiff lastResult results xs
	| otherwise =
		let newResults = res : results
		in collect' timeDiff (nextStartTime, price, price, price, price, amount) newResults xs
	where
		nextStartTime = CT.addToClockTime timeDiff curStartTime


collect scale ((ctime, price, amount):xs) =
	let
		(CT.CalendarTime y m d h min sec a b c dd e f) = CT.toUTCTime(ctime)
		(startCTTime, timeDiff) = case scale of
									Minute -> ((CT.CalendarTime y m d h min 0 a b c dd e f), (CT.TimeDiff 0 0 0 0 1 0 0))
									Hour ->   ((CT.CalendarTime y m d h 0 0 a b c dd e f), (CT.TimeDiff 0 0 0 1 0 0 0))
									Day ->    ((CT.CalendarTime y m d 0 0 0 a b c dd e f), (CT.TimeDiff 0 0 1 0 0 0 0))
									Week ->   ((CT.CalendarTime y m d 0 0 0 a b c dd e f), (CT.TimeDiff 0 0 7 0 0 0 0))
									Month -> ((CT.CalendarTime y m 0 0 0 0 a b c dd e f), (CT.TimeDiff 0 1 0 0 0 0 0))

		startClTime = CT.toClockTime startCTTime
		collected = collect' timeDiff (startClTime, price, price, price, price, amount) [] xs
	in reverse collected


convert ::  (CT.ClockTime, Price, Price, Price, Price, Int) -> String
convert (clockTime, openPrice, closePrice, minPrice, maxPrice, totalAmount) =
	T.printf "(%s, %s, %s, %s, %s, %s)" utcTime (show openPrice) (show closePrice) (show minPrice) (show maxPrice) (show totalAmount)
	where
		utcTime = CT.calendarTimeToString . CT.toUTCTime $ clockTime
	
test = map convert (collect Hour testData)
	
	
	
toData (y, m, d, h, mm, price, amount) =
	let calendarTime = CT.CalendarTime y m d h mm 0 0 CT.Sunday 0 "" 0 True
	in (CT.toClockTime calendarTime, price, amount)
	
	
testData = map toData [ 
	( 2011, CT.April, 10, 10, 31, 1.21, 1200 ),
	( 2011, CT.April, 10, 10, 52, 1.22, 400 ),
	( 2011, CT.April, 10, 11, 16, 1.24, 1300 ),
	( 2011, CT.April, 10, 11, 21, 1.23, 1000 ),
	( 2011, CT.April, 10, 11, 48, 1.26, 1200 ),
	( 2011, CT.April, 10, 12, 20, 1.25, 300 ),
	( 2011, CT.April, 10, 13, 05, 1.22, 200 ),
	( 2011, CT.April, 10, 13, 44, 1.23, 2000 ),
	( 2011, CT.April, 10, 14, 01, 1.24, 1400 ),
	( 2011, CT.April, 10, 14, 13, 1.25, 500 ),
	( 2011, CT.April, 10, 14, 22, 1.24, 700 ),
	( 2011, CT.April, 10, 14, 37, 1.23, 1100 ),
	( 2011, CT.April, 10, 14, 39, 1.22, 1500 ),
	( 2011, CT.April, 10, 15, 04, 1.22, 200 ),
	( 2011, CT.April, 10, 16, 32, 1.24, 400 ),
	( 2011, CT.April, 11, 22, 10, 1.33, 544 ),
	( 2011, CT.April, 11, 23, 45, 1.35, 1500 ),
	( 2011, CT.April, 11, 23, 49, 1.21, 1210 ),
	( 2011, CT.May,   1 , 5 , 14, 1.19, 778 ),
	( 2011, CT.May,   1 , 7 , 22, 1.16, 512 ),
	( 2011, CT.May,   1 , 8 , 0 , 1.28, 1445 ),
	( 2011, CT.May,   1 , 8 , 10, 1.29, 2144 )]
 