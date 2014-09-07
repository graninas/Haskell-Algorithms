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
    | nc == metaFactor   = (0,  snd $ rule' u)
    | nc <  metaFactor   = (-1, snd $ rule' u)
    | otherwise          = (1,  snd $ rule' u)
  where
    nc = length $ filter (\(_, c) -> c == Alive) (metaNeighbours u)

metaNeighbours :: (Universe2 a) -> [a]
metaNeighbours u =
    [ nearest5 . extract . left . left
    , pure     . extract . left . left  . extract . left
    , pure     . extract . right . right . extract . left
    , pure     . extract . left . left  . extract
    , pure     . extract . right . right . extract
    , pure     . extract . left . left  . extract . right
    , pure     . extract . right . right . extract . right
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

nearest3 :: Universe a -> [a]
nearest3 u = fmap extract [left u, u, right u]

neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)

fromList :: a -> [a] -> Universe a
fromList d (x:xs) = Universe (repeat d) x (xs ++ repeat d)

fromList2 :: a -> [[a]] -> Universe2 a
fromList2 d = Universe2 . fromList ud . fmap (fromList d)
    where ud = Universe (repeat d) d (repeat d)

cells = [ [ Dead, Alive,  Dead]
        , [Alive,  Dead,  Dead]
        , [Alive, Alive, Alive] ]

cells' = [ [Alive, Dead, Alive, Alive, Alive, Alive ]
        , [Dead, Alive, Alive, Alive, Dead, Alive ]
        , [Alive, Dead, Dead, Alive, Alive, Alive ]
        , [Dead, Alive, Dead, Dead, Alive, Dead ]
        , [Alive, Dead, Dead, Alive, Alive, Dead ]
        , [Dead, Alive, Alive, Dead, Dead, Alive ]
        , [Alive, Dead, Alive, Alive, Alive, Alive ]
        , [Dead, Alive, Alive, Dead, Alive, Alive ]
        , [Alive, Dead, Alive, Dead, Alive, Alive ]
        , [Dead, Alive, Dead, Dead, Alive, Dead ]
        , [Alive, Dead, Alive, Alive, Dead, Dead ]
        , [Dead, Alive, Alive, Dead, Alive, Alive ]
        , [Alive, Dead, Dead, Alive, Dead, Dead ]
        ]

initialModel :: Universe2 Cell
initialModel = fromList2 Dead cells

stepLifeUniverse = (=>> rule)
