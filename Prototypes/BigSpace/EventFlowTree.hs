module EventFlowTree where

import Types
import Tools
import Event

-- | Создает дерево с нужным количеством уровней. В листах ставится событие templEvnt.
mkTree :: (Int, Int) -> [Int] -> Event -> EventFlowTree
mkTree (0, _) (t:_) (_, event, str) = Leaf (t, event, str)
mkTree (lvls, leafsCnt) ticks templEvnt =
		let
			leftBound = head ticks
			rightBound = last ticks
			middle = leafsCnt `div` 2
			leftTicks = take middle ticks
			rightTicks = drop middle ticks
		in Branch (leftBound, rightBound) (mkTree (lvls - 1, middle) leftTicks templEvnt) (mkTree (lvls - 1, middle) rightTicks templEvnt)


-- | Создает пустое двоичное дерево. Листьев - столько, сколько нужно, чтобы вместить minLeafsCnt.
-- | lvls - количество Branch-уровней, степень двойки.
-- | leafsCnt - количество листьев, двойка возведенная в степень.
mkEmptyTree :: Int -> EventFlowTree
mkEmptyTree minLeafsCnt =
					let
						lls@(_, leafsCnt) = head [(pow, x) | (pow, x) <- numberPowList 2, x >= minLeafsCnt]
						ticks = [0..leafs-1]
					in mkTree lls ticks emptyEvent


update :: EventFlowTree -> Event -> EventFlowTree
update (Branch tf
			l@(Leaf (tickL, _, _))
			r)
		event@(eTick, _, _) =
			case tickL == eTick of
				True  -> Branch tf (Leaf event) r
				False -> Branch tf l (Leaf event)

update (Branch tf
			l@(Branch (_, lr) _ _)
			r)
		event@(eTick, _, _) =
			case lr < eTick of
				True  -> Branch tf l (update r event)
				False -> Branch tf (update l event) r

fromTimeLine :: Int -> TimeLine -> EventFlowTree
fromTimeLine minLeafsCnt tl = let tree = mkEmptyTree minLeafsCnt in undefined
					