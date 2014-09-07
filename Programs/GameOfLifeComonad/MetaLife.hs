{-# LANGUAGE TupleSections #-}

module MetaLife where

import Control.Comonad
import Control.Applicative

import Universe

data Cell =  Dead | Alive
    deriving (Eq, Show)
    
type MetaCell = (Int, Cell)


metaFactor = 3

metaRule :: Universe2 MetaCell -> MetaCell
metaRule u
    | nc == metaFactor   = rule' u
    | nc <  metaFactor   = (-1, snd $ rule' $ u)
    | otherwise          = (1,  snd $ rule' $ u)
  where
    nc = length $ filter (\(_, c) -> c == Alive) (metaNeighbours u)

metaNeighbours :: (Universe2 a) -> [a]
metaNeighbours u =
    [ nearest5 . extract . left . left
    , pure     . extract . left . left  . extract . left -- 3
    , pure     . extract . right . right . extract . left -- 4
    , pure     . extract . left . left  . extract -- 1
    , pure     . extract . right . right . extract -- 2
    , pure     . extract . left . left  . extract . right -- 5
    , pure     . extract . right . right . extract . right -- 6
    , nearest5 . extract . right . right
    ] >>= ($ getUniverse2 u)

nearest5 :: Universe a -> [a]
nearest5 u = fmap extract [left . left $ u, left u, u, right u, right . right $ u]

rule' :: Universe2 MetaCell -> MetaCell
rule' u
    | nc == (factor + 2) = oldMetaCell
    | nc == (factor + 3) = (factor, Alive)
    | otherwise          = (factor, Dead)
    where
        oldMetaCell@(factor, oldCell) = extract u
        nc = length $ filter (\(_, c) -> c == Alive) (neighbours u)

metaCells = map (map (0,)) cells

initialMetaModel :: Universe2 MetaCell
initialMetaModel = fromList2 (0, Dead) metaCells

stepMetaLifeUniverse = (=>> metaRule)

----------------------- Original Life:


nearest3 :: Universe a -> [a]
nearest3 u = fmap extract [left u, u, right u]

neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)

rule :: Universe2 Cell -> Cell
rule u
    | nc == 2   = extract u
    | nc == 3   = Alive
    | otherwise = Dead
    where nc = length $ filter (==Alive) (neighbours u)

fromList :: a -> [a] -> Universe a
fromList d (x:xs) = Universe (repeat d) x (xs ++ repeat d)

fromList2 :: a -> [[a]] -> Universe2 a
fromList2 d = Universe2 . fromList ud . fmap (fromList d)
    where ud = Universe (repeat d) d (repeat d)

cells' = [ [ Dead, Alive,  Dead]
        , [Alive,  Dead,  Dead]
        , [Alive, Alive, Alive] ]

cells = [[Alive, Alive],
         [Alive, Alive]]

initialModel :: Universe2 Cell
initialModel = fromList2 Dead cells

stepLifeUniverse = (=>> rule)
