{-# LANGUAGE TupleSections #-}

module MetaMetaLife where

import Control.Comonad
import Control.Applicative

import Universe

data Cell =  Dead | Alive
    deriving (Eq, Show)
    
type MetaCell = (Int, Cell)
type MetaMetaCell = (Int, MetaCell)

factor'' = 2
factor' = 13

isAlive (_, (_, c)) = c == Alive
zeroCellCreator c = (0, (0, c))
zeroCell :: MetaMetaCell
zeroCell = zeroCellCreator Dead

rule'' :: Universe2 MetaMetaCell -> MetaMetaCell
rule'' u
    | nc == factor''   = (0,  snd $ rule' u)
    | nc <  factor''   = (1,  snd $ rule' u)
    | otherwise        = (-1, snd $ rule' u)
  where
    nc = length $ filter isAlive (neighbours'' u)

rule' :: Universe2 MetaMetaCell -> MetaMetaCell
rule' u
    | nc == factor' = old
    | otherwise     = (factModifier'', (factModifier' + factModifier'', snd . snd $ rule u))
  where
    old@(factModifier'', (factModifier', c)) = extract u
    nc = length $ filter isAlive (neighbours' u)

rule :: Universe2 MetaMetaCell -> MetaMetaCell
rule u
    | nc == (f' + 2) = old
    | nc == (f' + 3) = (f'', (f', Alive))
    | otherwise      = (f'', (f', Dead))
    where
        old@(f'', (f', c)) = extract u
        nc = length $ filter isAlive (neighbours u)


neighbours'' :: (Universe2 a) -> [a]
neighbours'' u =
    [ nearest7 . extract . left . left . left
    , pure     . extract . left . left . left . extract . left
    , pure     . extract . right . right . right . extract . left
    , pure     . extract . left . left . left . extract . left . left
    , pure     . extract . right . right . right . extract . left . left
    , pure     . extract . left . left . left . extract
    , pure     . extract . right . right . right . extract
    , pure     . extract . left . left . left . extract . right
    , pure     . extract . right . right . right . extract . right
    , pure     . extract . left . left . left . extract . right . right
    , pure     . extract . right . right . right . extract . right . right
    , nearest7 . extract . right . right . right
    ] >>= ($ getUniverse2 u)

neighbours' :: (Universe2 a) -> [a]
neighbours' u =
    [ nearest5 . extract . left . left
    , pure     . extract . left . left  . extract . left
    , pure     . extract . right . right . extract . left
    , pure     . extract . left . left  . extract
    , pure     . extract . right . right . extract
    , pure     . extract . left . left  . extract . right
    , pure     . extract . right . right . extract . right
    , nearest5 . extract . right . right
    ] >>= ($ getUniverse2 u)
    
neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)

initialModel'' :: Universe2 MetaMetaCell
initialModel'' = fromList2 zeroCell metaCells

stepLifeUniverse'' = (=>> rule'')

metaCells = map (map zeroCellCreator) cells

cells' = [ [ Dead, Alive,  Dead]
        , [Alive,  Dead,  Dead]
        , [Alive, Alive, Alive] ]

cells = [ [Alive, Dead, Alive, Alive, Alive, Alive ]
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

