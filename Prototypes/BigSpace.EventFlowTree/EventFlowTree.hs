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
						ticks = [0..leafsCnt-1]
					in mkTree lls ticks emptyEvent


update :: Event -> EventFlowTree -> EventFlowTree
update event@(eTick, _, _) 
		(Branch tf
			l@(Leaf (tickL, _, _))
			r) =
			case tickL == eTick of
				True  -> Branch tf (Leaf event) r
				False -> Branch tf l (Leaf event)

update event@(eTick, _, _)
		(Branch tf
			l@(Branch (_, lr) _ _)
			r) =
			case lr < eTick of
				True  -> Branch tf l (update event r)
				False -> Branch tf (update event l) r

lookupByEvent :: Char -> EventFlowTree -> Maybe Event
lookupByEvent ch (Leaf e@(t, ech, _)) | ech == ch = Just e
									  | otherwise = Nothing
lookupByEvent ch (Branch _ l r) = case lookupByEvent ch l of
									Just e -> Just e
									Nothing -> lookupByEvent ch r

fromTimeLine :: Int -> TimeLine -> EventFlowTree
fromTimeLine minLeafsCnt tl = foldr update (mkEmptyTree minLeafsCnt) tl
					