module Main where

import qualified Data.List as L

type Cell = (Int, Int)
type Shape = [Cell]

tripleOscilator :: Shape
tripleOscilator = [ (0, 0)
                  , (0, 1)
                  , (0, 2)
                  ]

glider :: Shape
glider = [ (0, 0)
         , (1, 0)
         , (2, 0)
         , (2, 1)
         , (1, 2)
         ]

shift :: Cell -> Shape -> Shape
shift (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

(>|<) :: Shape -> Shape -> Shape
sh1 >|< sh2 = L.nub (sh1 ++ sh2)

iterations :: Shape -> [Shape]
iterations pos = pos : iterations (step pos)

neighbours8 cell cells = cells `L.intersect` (box8 cell)

box8 (ax, ay) = [(x,y) | x <- range ax, y <- range ay, (ax,ay) /= (x,y)]
lim n = [n - 1, n + 1]
range n = [n - 1, n, n + 1]

step :: Shape -> Shape
step p = let
    next all [] = []
    next all cur@((aX, aY) : alives) = 
        [(x, y) | x <- lim aX, y <- lim aY,
                  length (neighbours8 (x, y) all) == 3]
        ++ (next all alives)
    alive all cell = length (neighbours8 cell all) `elem` [2,3]
  in L.nub $ filter (alive p) p ++ (next p p)


whileAlive :: Shape -> [Shape]
whileAlive p = let next = step p
               in if (next == p)
                    then p : []
                    else p : whileAlive next


