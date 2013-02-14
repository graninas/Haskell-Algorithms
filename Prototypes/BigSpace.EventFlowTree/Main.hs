module Main where

import Types
import EventFlowTree
import PreGen.World
import Actions

run :: Int -> EventFlowTree -> EventFlowTree -> IO ()
run tt preGenTree tree = do
		putStr (show tt ++ "tt ")
		act <- getLine
		case act of
			"send" -> let (t, _, _) = findEvent 'r' preGenTree in
						case sendToFuture t tree of
							Just newTree -> do
								putStrLn "SpaceShip sended to future."
								run (tt + 1) preGenTree newTree
							Nothing -> putStrLn "No send." >> run (tt + 1) preGenTree tree
			_ -> run (tt + 1) preGenTree tree




main :: IO ()
main = do
		putStrLn "BigSpace"

		let gameSeconds = 31104000
		run 0 (fromTimeLine gameSeconds preGenWorldTL) $ mkEmptyTree gameSeconds