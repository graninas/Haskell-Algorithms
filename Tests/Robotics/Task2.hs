module Main where

import Data.List (nub, partition, sort, group)
import Data.Word

type Field = [[Char]]

u = 'u'
d = 'd'
r = 'r'
l = 'l'

field1 = [ [d, r, u, r, l]
         , [l, u, d, l, r]
         , [l, d, d, r, r]
         , [d, r, u, l, u]
         , [u, d, r, r, d] ]

field2 = [ [d, r, u, r, l]
         , [r, u, d, l, r]
         , [r, r, d, r, r]
         , [d, r, u, l, u]
         , [u, d, r, r, d] ]

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

move :: Field -> Dislocation -> Position -> Dislocation
move f (tink, kiki) newTink@(tx, ty) = let
    kikiAction = (f !! tx) !! ty
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

type Deep = Word

solve :: Field -> Deep -> Dislocation -> SolutionTree
solve f 1 d@(tink, kiki) | tink == kiki = SL d
                         | otherwise = L d
solve f deep d@(tink, kiki) | tink == kiki = SL d
                            | otherwise = B d $ map (solve f newDeep) nextDislocations
  where
    newDeep = deep - 1
    nextDislocations = map (move f d) (possibleMoves tink)

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

-- �������� ���������� ������� ��� ������� n
try f n = let ss = foldSuccesses (solve f n startPositions)
              pathLengths = map length ss
              groupedPathLengths = group . sort $ pathLengths
              pathLengthStats = map (\gpls -> (head gpls, length gpls)) groupedPathLengths
          in (length ss, pathLengthStats)

-- �������� ������� ������ k ��� ������� n
solutions f k n = let ss = foldSuccesses (solve f n startPositions)
                      kLengths = filter ((== k) . length) ss
                  in kLengths

goodSolution = solutions field1 6 10