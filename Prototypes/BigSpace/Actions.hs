module Actions where

import Types
import EventFlowTree
import Event

sendToFuture :: Int -> EventFlowTree -> Maybe EventFlowTree
sendToFuture 0 tree = Nothing
sendToFuture tick tree = Just $ update (tick, 'r', "SpaceShip sended to past and returned.") tree


findEvent :: Char -> EventFlowTree -> Event
findEvent ch tree = case lookupByEvent ch tree of
						Just e -> e
						Nothing -> emptyEvent