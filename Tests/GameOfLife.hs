module Main where

import qualified Data.List as L

type Shape = [(Integer, Integer)]

tripleOscilator :: Shape
tripleOscilator = [ (0, 0)
                  , (0, 1)
                  , (0, 2)
                  ]

glider = [ (0, 0)
         , (1, 0)
         , (2, 0)
         , (2, 1)
         , (1, 2)
         ]

shift :: (Integer, Integer) -> Shape -> Shape
shift (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

(>|<) :: Shape -> Shape -> Shape
sh1 >|< sh2 = L.nub (sh1 ++ sh2)

iterations :: Shape -> [Shape]
iterations pos = pos : iterations (next pos)
    where
        next p = L.nub $ filter (stillAlive p) p ++ (next' p p)
        next' all [] = []
        next' all cur@((aX, aY) : alives) = [(x, y) | x <- lim aX, y <- lim aY,
                                                length (neighbours8 (x, y) all) == 3]
                                     ++ (next' all alives)
        box8 (ax, ay) = [(x,y) | x <- range ax, y <- range ay, (ax,ay) /= (x,y)]
        --box9 (ax, ay) = [(x,y) | x <- range ax, y <- range ay]
        lim n = [n - 1, n + 1]
        range n = [n - 1..n + 1]
        --neighbours9 cell cells = cells `L.intersect` (box9 cell)
        neighbours8 cell cells = cells `L.intersect` (box8 cell)
        stillAlive all cell = length (neighbours8 cell all) `elem` [2,3]

