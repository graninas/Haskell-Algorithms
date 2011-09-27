import Data.List
import Data.String.Utils
import Criterion.Main (defaultMain, bench, whnf)

testUnlines n = unlines . take n $ (repeat "klsdfldskfjdks")
testIntercalate n = intercalate "\n\r" . take n $ (repeat "klsdfldskfjdks")

main = defaultMain [
		  bench "testUnlines 10" $ whnf  testUnlines (10)
		, bench "testIntercalate 10" $ whnf testIntercalate (10)
		, bench "testUnlines 100" $ whnf  testUnlines (100)
		, bench "testIntercalate 100" $ whnf testIntercalate (100)
		, bench "testUnlines 1000" $ whnf  testUnlines (1000)
		, bench "testIntercalate 1000" $ whnf testIntercalate (1000)
		, bench "testUnlines 10000" $ whnf  testUnlines (10000)
		, bench "testIntercalate 10000" $ whnf testIntercalate (10000)
		, bench "testUnlines 50000" $ whnf testUnlines (50000)
		, bench "testIntercalate 50000" $ whnf testIntercalate (50000)
		, bench "testUnlines 100000" $ whnf testUnlines (100000)
		, bench "testIntercalate 100000" $ whnf testIntercalate (100000)
		, bench "testUnlines 200000" $ whnf testUnlines (200000)
		, bench "testIntercalate 200000" $ whnf testIntercalate (200000)
       ]

{-
Benchmark resuts (means):
Items    testUnlines    testIntercalate    Percent
10       23.84          34.05              29.9
100      22.70          34.62              34.4
1000     23.28          35.48              34.3
10000    22.17          35.48              37.5
50000    22.13          33.26              33.4
100000   21.06          35.47              40.6
200000   22.70          34.05              33.3
-}