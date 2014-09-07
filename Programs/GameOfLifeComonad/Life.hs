{-
    This code taken from Habrahabr:
    http://habrahabr.ru/post/225473/
-}

module Life where

import Control.Comonad
import Control.Applicative

import Universe

data Cell = Dead | Alive
    deriving (Eq, Show)

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

cells = [ [ Dead, Alive,  Dead]
        , [Alive,  Dead,  Dead]
        , [Alive, Alive, Alive] ]

initialModel :: Universe2 Cell
initialModel = fromList2 Dead cells

stepLifeUniverse = (=>> rule)
