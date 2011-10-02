module Main where

import Types
import EventFlowTree
import PreGen.World

run :: Int -> TimeLine -> EventFlowTree -> IO ()
run tt tl tree = do
		putStr (show tt ++ "tt ")
		act <- getLine
		run (tt + 1) tl tree




main :: IO ()
main = do
		putStrLn "BigSpace"
		run 0 preGenWorldTL $ mkEmptyTree 31104000