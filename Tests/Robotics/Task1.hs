{- Дано конечное ограниченное поле 5х5, в каждой ячейке которого имеется одна стрелка: вверх, вниз, вправо или влево.
   На позицию (x1, y1) помещается робот Тинк.
   На позицию (x2, y2) помещается роботиха Кики.
   Тинк движется по полю. Стрелка, на которую он зашел, указывает Кики, куда ей двигаться.
   Если движение невозможно (за край поля выйти нельзя), Кики остается на месте.
   
   Задача: найти все решения для n шагов, когда Кики встречается с Тинком.
-}

module Main where

import Data.List (nub, partition, sort, group)

u = 'u'
d = 'd'
r = 'r'
l = 'l'

field = [ [u, l, u, r, l]
        , [u, d, d, r, r]
        , [l, r, r, l, l]
        , [d, r, u, u, u]
        , [d, d, r, l, u] ]

type Position = (Int, Int)
type Positions = [Position]
type Dislocation = (Position, Position)

tinkStart, kikiStart :: Position
tinkStart = (0, 0)
kikiStart = (4, 4)
startPositions :: Dislocation
startPositions = (tinkStart, kikiStart)

normalize x | x < 0 = 0
            | x > 4 = 4
            | otherwise = x

inBounds x | x < 0 = False
           | x > 4 = False
           | otherwise = True

moveKiki :: Position -> Char -> Position
moveKiki (kx, ky) 'u' = (normalize $ kx - 1, ky)
moveKiki (kx, ky) 'd' = (normalize $ kx + 1, ky)
moveKiki (kx, ky) 'l' = (kx, normalize $ ky - 1)
moveKiki (kx, ky) 'r' = (kx, normalize $ ky + 1)

move :: Dislocation -> Position -> Dislocation
move (tink, kiki) newTink@(tx, ty) = let
    kikiAction = (field !! tx) !! ty
    newKiki = moveKiki kiki kikiAction
    in (newTink, newKiki)

possibleMoves :: Position -> Positions
possibleMoves (x, y) = filter validCoords cross
  where
    validCoords (a, b) = inBounds a && inBounds b
    cross = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


data SolutionTree = SL Dislocation
                  | L Dislocation
                  | B Dislocation [SolutionTree]
  deriving (Show, Read, Eq)


solve :: Int -> Dislocation -> SolutionTree
solve 1 d@(tink, kiki) | tink == kiki = SL d
                       | otherwise = L d
solve ttl d@(tink, kiki) | tink == kiki = SL d
                         | otherwise = B d $ map (solve newTtl) nextDislocations
  where
    newTtl = ttl - 1
    nextDislocations = map (move d) (possibleMoves tink)

type Path = [Dislocation]
type Paths = [Path]

foldSuccesses :: SolutionTree -> Paths
foldSuccesses (SL d) = [[d]]
foldSuccesses (L _) = []
foldSuccesses (B d branches) = case folded branches of
    [] -> []
    sucPaths -> map (d:) sucPaths
  where
    folded :: [SolutionTree] -> Paths
    folded [] = []
    folded bs = concat $ map foldSuccesses bs

-- Показать статистику решений для глубины n
try n = let ss = foldSuccesses (solve n startPositions)
            pathLengths = map length ss
            groupedPathLengths = group . sort $ pathLengths
            pathLengthStats = map (\gpls -> (head gpls, length gpls)) groupedPathLengths
        in (length ss, pathLengthStats)

-- Показать решения длиной k для глубины n
solutions k n = let ss = foldSuccesses (solve n startPositions)
                    kLengths = filter ((== k) . length) ss
                in kLengths