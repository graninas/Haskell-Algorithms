module Main where

import qualified Data.List as L
import qualified Control.Monad as M

type ListData = (Char, Float)

devices :: [ListData]
devices = [ ('A', 0.37)
		  , ('B', 0.43)
		  , ('C', 0.2)
		  ]

failureCost :: [Float]
failureCost = [ 20, 20, 30, 30 ]

variations = L.permutations devices

sums [] = 0
sums ((_,pa):aas) = pa + sums aas

sums' :: [ListData] -> Float
sums' = foldr (\(_, p) -> (p +)) 0

variantProbability [] = ("", 1)
variantProbability ((dev, p):xs) = let
	(devs, probs) = variantProbability xs
	prob          = p / (p + sums xs)
	in (dev : devs, prob * probs)

variantProbability' [] = ("", 1)
variantProbability' d@((dev, p) : xs) = let
	(devs, probs) = variantProbability' xs
	prob          = (p / sums d)
	in (dev : devs, prob * probs)

variantProbability'' xs = f xs (sums xs)
  where
	f [] _ = ("", 1)
	f ((dev, p):xs) sum = let
		(devs, probs) = f xs (sum - p)
		prob          = (p / sum)
		in (dev : devs, prob * probs)

variantProbabilities   = map variantProbability   variations
variantProbabilities'  = map variantProbability'  variations
variantProbabilities'' = map variantProbability'' variations -- Немного другой результат!

-- [("ABC",0.2525397),("BAC",0.27912283),("CBA",0.10750001),("BCA",0.1508772),("CAB",9.25e-2),("ACB",0.117460325)]
-- [("ABC",0.2525397),("BAC",0.27912283),("CBA",0.10750001),("BCA",0.1508772),("CAB",9.25e-2),("ACB",0.117460325)]
-- [("ABC",0.25253972),("BAC",0.27912286),("CBA",0.10750001),("BCA",0.1508772),("CAB",9.25e-2),("ACB",0.117460325)]


cell xs c@(dev, pos) = (c, sum devProbs')
  where
	devProbs = [snd varProb | varProb <- xs, pos `elem` (L.elemIndices dev . fst $ varProb)]
	devProbs' = do
		varProb <- xs
		M.guard (pos `elem` (L.elemIndices dev . fst $ varProb))
		return (snd varProb)

row dev = do
	col <- [0..(length failureCost) - 1]
	return (cell variantProbabilities (dev, col))

table = do
	dev <- map fst devices
	return (row dev)

damage dev = let
	zipped = zipWith (\x y -> snd x * y) (row dev) failureCost
	in sum zipped

damages = map damage (map fst devices)

{-
failureCost = [ 20, 20, 30, 30 ]

	[ (('A',0),0.37),(('A',1),0.37162283),(('A',2),0.2583772)
	, (('B',0),0.43),(('B',1),0.3600397), (('B',2),0.20996033)
	, (('C',0),0.20000002),(('C',1),0.26833752),(('C',2),0.5316625)
	]
	
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function. For example, zipWith (+) is applied to two lists to produce the list of corresponding sums. 
-}