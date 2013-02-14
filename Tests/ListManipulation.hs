import Data.List
import Criterion.Main (defaultMain, bench, whnf)

takeInterest :: [String] -> [Int] -> [String]
takeInterest _ [] = []
takeInterest ss (n:ns) = [ss !! n] ++ takeInterest ss ns

collectFields :: Int -> [Int] -> [String] -> [String]
collectFields _ _ [] = []
collectFields idx fis (s:ss) | idx `elem` fis = s : collectFields (idx+1) fis ss
collectFields idx fis (s:ss) | otherwise = collectFields (idx+1) fis ss

testDataList n = replicate n "String!"
testDataIndexes n = [0..n-1]

iterestFunc n = takeInterest (testDataList n) (testDataIndexes n)
collectFunc n = collectFields 0 (testDataIndexes n) (testDataList n)

main = defaultMain [
		bench "takeInterest 10"     $ whnf iterestFunc 10
		, bench "collectFields 10"  $ whnf collectFunc 10

		, bench "takeInterest 100"    $ whnf iterestFunc 100
		, bench "collectFields 100"   $ whnf collectFunc 100

		, bench "takeInterest 1000"   $ whnf iterestFunc 1000
		, bench "collectFields 1000"  $ whnf collectFunc 1000

		, bench "takeInterest 10000"  $ whnf iterestFunc 10000
		, bench "collectFields 10000" $ whnf collectFunc 10000

		, bench "takeInterest 50000"  $ whnf iterestFunc 50000
		, bench "collectFields 50000" $ whnf collectFunc 50000
       ]

{-
Benchmark resuts (means):
Items    takeInterest   collectFields      Percent
10       17.33          36.84              52.9
100      20.58          36.84              44.1
1000     21.67          37.92              42.8
10000    21.13          36.84              42.6
50000    21.67          37.92              42.8
-}