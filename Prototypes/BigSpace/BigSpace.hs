module Main where

import Type.Types
import Type.Event

data WorldState a = WS
	{
		eventTree :: [Event a]
	}
	deriving (Show)

zeroWorldState :: WorldState Int
zeroWorldState = WS {eventTree = []}



main = do

	x <- return zeroWorldState
	putStrLn "Done."