module Types where

type Event = (Int, Char, String)
type TimeFlow = (Int, Int)
type TimeLine = [Event]

data EventFlowTree = Leaf Event
					| Branch TimeFlow EventFlowTree EventFlowTree
	deriving (Show)