module Main where

percent :: Float -> Float -> ([Float], Float) -> ([Float], Float)
percent p bound (partResList, num) | num > bound =
		let
			newNum = num - (num * p)
			newResList = partResList ++ [newNum]
		in percent p bound (newResList, newNum)

percent _ _ res | otherwise = res

exerciseData :: [(Float, Float, Float)]
exerciseData = [(0.1, 500000.0, 99.0),
				(0.05, 400000.0, 99.0),
				(0.1, 250.0, 1.0),
				(0.05, 250.0, 1.0)]

resFileName = "SlyPercents.txt"


eval :: (Float, Float, Float) -> ([Float], Float)
eval (p, num, bound) = percent p bound ([num], num)

main = do
	writeFile resFileName ""
	let results = map eval exerciseData
	appendFile resFileName (show results)

	putStrLn "Done."