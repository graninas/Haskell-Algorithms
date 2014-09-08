{-# LANGUAGE TupleSections #-}

module MetaLife where

import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Vector as V

import Universe

type Cell = Int
dead  = 0
alive = 1

type MetaCell = (Int, Cell)
type MetaMetaCell = (Int, MetaCell)
type MetaFactor = (Int, Int)

isalive (_, (_, c)) = c == alive
zeroCellCreator c = (0, (0, c))
zeroCell :: MetaMetaCell
zeroCell = zeroCellCreator dead

rule'' :: MetaFactor -> Universe2 MetaMetaCell -> MetaMetaCell
rule'' mf@(f'', f') u
    | nc == f''   = (0,  new)
    | nc <  f''   = (1,  new)
    | otherwise   = (-1, new)
  where
    new = snd $ rule' mf u
    nc = length $ filter isalive (parNeighbours neighbours'' u)

rule' :: MetaFactor -> Universe2 MetaMetaCell -> MetaMetaCell
rule' mf@(f'', f') u
    | nc <  (f' - factModifier'' - factModifier'')  = (factModifier'', (0,  new))
    | nc == f'                                      = (factModifier'', (1,  new))
    | nc >  (f' + factModifier'' + factModifier'')  = (factModifier'', (-1, new))
    | otherwise                                     = (factModifier'', (0,  new))
  where
    old@(factModifier'', (factModifier', c)) = extract u
    new = snd . snd $ rule mf u
    nc = length $ filter isalive (parNeighbours neighbours' u)

rule :: MetaFactor -> Universe2 MetaMetaCell -> MetaMetaCell
rule mf@(f'', f') u
    | nc == (f' + 2) = old
    | nc == (f' + 3) = (f'', (f', alive))
    | otherwise      = (f'', (f', dead))
    where
        old@(f'', (f', c)) = extract u
        nc = length $ filter isalive (parNeighbours neighbours u)


parNeighbours :: NFData a => ((Universe2 a) -> [a]) -> (Universe2 a) -> [a]
parNeighbours ns u = runEval $ parList rpar (force $ ns u)
   
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

initialModel :: Int -> Universe2 MetaMetaCell
initialModel s = fromList2 s zeroCell metaCells

stepLifeUniverse'' mf = (=>> (rule'' mf))

metaCells = map (map zeroCellCreator) cells

cells' = [[alive, alive, dead, alive, alive]]

cells'' = [[alive, alive, alive]]

cells = [ [ dead, alive,  dead]
        , [alive,  dead,  dead]
        , [alive, alive, alive] ]

cells''' = [ [alive, dead, alive, alive, alive, alive ]
        , [dead, alive, alive, alive, dead, alive ]
        , [alive, dead, dead, alive, alive, alive ]
        , [dead, alive, dead, dead, alive, dead ]
        , [alive, dead, dead, alive, alive, dead ]
        , [dead, alive, alive, dead, dead, alive ]
        , [alive, dead, alive, alive, alive, alive ]
        , [dead, alive, alive, dead, alive, alive ]
        , [alive, dead, alive, dead, alive, alive ]
        , [dead, alive, dead, dead, alive, dead ]
        , [alive, dead, alive, alive, dead, dead ]
        , [dead, alive, alive, dead, alive, alive ]
        , [alive, dead, dead, alive, dead, dead ]
        ]


