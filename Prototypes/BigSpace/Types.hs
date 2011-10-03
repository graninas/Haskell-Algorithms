module Types where

type Event = (Int, Char, String)
type TimeFlow = (Int, Int)
type TimeLine = [Event]

data EventFlowTree = Leaf Event
					| Branch TimeFlow EventFlowTree EventFlowTree
-- Если использовать энергичный вариант, то при отправке корабля в будущее программа крашится из-за нехватки памяти.
-- Ленивый вариант тоже отжирает память, но не крашится.
--					| Branch TimeFlow !EventFlowTree !EventFlowTree
	deriving (Show)