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

isAlive (_, (_, c)) = c == alive
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
    nc = length $ filter isAlive (parNeighbours neighbours'' u)

rule' :: MetaFactor -> Universe2 MetaMetaCell -> MetaMetaCell
rule' mf@(f'', f') u
    | nc <  (f' - factModifier' - factModifier'')  = (factModifier'', (0,  new))
    | nc == f'                                      = (factModifier'', (1,  new))
    | nc >  (f' + factModifier' + factModifier'')  = (factModifier'', (-1, new))
    | otherwise                                     = (factModifier'', (0,  new))
  where
    old@(factModifier'', (factModifier', c)) = extract u
    new = snd . snd $ rule mf u
    nc = length $ filter isAlive (parNeighbours neighbours' u)

rule :: MetaFactor -> Universe2 MetaMetaCell -> MetaMetaCell
rule mf@(f'', f') u
    | nc == 2   = old
    | nc == 3   = (f'', (f', alive))
    | otherwise = (f'', (f', dead))
    where
        old@(f'', (f', c)) = extract u
        nc = length $ filter isAlive (parNeighbours neighbours u)


parNeighbours :: NFData a => ((Universe2 a) -> [a]) -> (Universe2 a) -> [a]
parNeighbours ns u = runEval $ parList rpar (force $ ns u)
   
neighbours'' :: (Universe2 a) -> [a]
neighbours'' u =
    [ nearest7 . extract . left3
    , pure     . extract . left3  . extract . left
    , pure     . extract . right3 . extract . left
    , pure     . extract . left3  . extract . left2
    , pure     . extract . right3 . extract . left2
    , pure     . extract . left3  . extract
    , pure     . extract . right3 . extract
    , pure     . extract . left3  . extract . right
    , pure     . extract . right3 . extract . right
    , pure     . extract . left3  . extract . right2
    , pure     . extract . right3 . extract . right2
    , nearest7 . extract . right3
    ] >>= ($ getUniverse2 u)

neighbours' :: (Universe2 a) -> [a]
neighbours' u =
    [ nearest5 . extract . left2
    , pure     . extract . left2  . extract . left
    , pure     . extract . right2 . extract . left
    , pure     . extract . left2  . extract
    , pure     . extract . right2 . extract
    , pure     . extract . left2  . extract . right
    , pure     . extract . right2 . extract . right
    , nearest5 . extract . right2
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

stepLifeUniverse'' mf = (=>> (rule mf))

metaCells = map (map zeroCellCreator) cellsGlider

cells' = [[alive, alive, dead, alive, alive]]

cells'' = [[alive, alive, alive]]

cellsGlider = [ [ dead, alive,  dead]
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

testLine  = [ alive : replicate 28 dead ++ [alive]]
testBlock = [ replicate 14 dead ++ [alive, alive] ++ replicate 14 dead]
fillers n = replicate n (replicate 30 dead)

testCells =  testLine
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testBlock
      ++ testBlock
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testLine

