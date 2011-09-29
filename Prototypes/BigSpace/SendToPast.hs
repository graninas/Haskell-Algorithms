module Main where

import qualified Data.List as L


type TimePos = Int

type TimeEvent = String

type TimeLine = [(TimePos, TimeEvent)]

stohastickTimeLine = [(0, "The begining"),
			(5, "Space ship from future!"),
			(10, "Nonexistence line"),
			(15, "SpaceShip done."),
			(16, "Sending back to past.")]


event :: Int -> TimeLine -> (String, String)
event t tl = case L.lookup t tl of
				Just te -> (tick ++ te, te)
				Nothing -> (tick ++ "No event.", "")
	where
		tick = show t ++ "t "

timeTick t tl realTl = do
		let (eventStr, e) = event t tl
		putStr eventStr
		act <- getLine
		case act of
			"" -> timeTick (t+1) tl ((t, e) : realTl)
			_ -> do
					putStrLn "Time over.\nPassed timeline:"
					mapM_ putStrLn . map show . reverse $ realTl
		
main = do
	putStrLn "Test."
	timeTick 0 stohastickTimeLine []