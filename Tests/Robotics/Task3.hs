module Main where

import Data.List (nub, partition, sort, group, elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Command = Char
type Commands = [Command]
type Field = [[Command]]
type Manipulator = [[Command]]

u, d, r, l :: Command
u = 'u'
d = 'd'
r = 'r'
l = 'l'

l1 = '1'
l2 = '2'
l3 = '3'
l4 = '4'
l5 = '5'
l6 = '6'
l7 = '7'
programLines = [l1, l2, l3, l4, l5, l6, l7]
programSize = length programLines

manipulator1 :: Manipulator
manipulator1 = [ [l3, r,  l1, l,  l4]
               , [u,  l1, d,  l2, u]
               , [l7, l,  l7, r,  l5]
               , [d,  l4, u,  l3, d]
               , [l2, r,  l6, l,  l6] ]

s = 's' -- space
p = 'p' -- plug

field1 :: Field
field1 = [ [p, s, s, s, p]
         , [s, s, s, s, s]
         , [s, s, s, s, s]
         , [s, s, s, s, s]
         , [p, s, s, s, p] ]

type Position = (Int, Int)
type Positions = [Position]
type Dislocation = (Position, Position)

tinkStart, kikiStart :: Position
tinkStart = (2, 2)
kikiStart = (2, 2)
startPositions :: Dislocation
startPositions = (tinkStart, kikiStart)

normalize x | x < 0 = 0
            | x > 4 = 4
            | otherwise = x

inBounds x | x < 0 = False
           | x > 4 = False
           | otherwise = True

possibleMoves :: Position -> Positions
possibleMoves (x, y) = filter validCoords cross
  where
    validCoords (a, b) = inBounds a && inBounds b
    cross = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

type Line = Int
type Program = Map.Map Line Command
                      
updateProgram :: Line -> Command -> Program -> Program
updateProgram line command = Map.update (\_ -> Just command) line

getCommand :: Manipulator -> Position -> Command
getCommand m (tx, ty) = (m !! tx) !! ty

getLine :: Manipulator -> Position -> Line
getLine m (tx, ty) = line
  where
    command = (m !! tx) !! ty
    line = fromJust . elemIndex command $ programLines

    
isProgramValid :: Program -> Bool
isProgramValid prog = all (== True) $ map (\k -> Map.member k prog) [1..programSize]

moveBounded :: Position -> Command -> Position
moveBounded (dx, dy) command | command == u = (normalize $ dx - 1, dy)
                             | command == d = (normalize $ dx + 1, dy)
                             | command == l = (dx, normalize $ dy - 1)
                             | command == r = (dx, normalize $ dy + 1)

type Path = [Position]

evalCommand :: Line -> Command -> (Position, Path) -> (Position, Path)
evalCommand _ command (pos, ps) = let newPos = moveBounded pos command
                                  in (newPos, ps ++ [newPos])

data Result = Result
                { resultPath :: Path
                , reversedPath :: Path }
            | Error
  deriving (Show, Read, Eq)

evalProgram :: Position -> Program -> Result
evalProgram robotPos prog | isProgramValid prog = let
    (newPos, path) = Map.foldWithKey evalCommand (robotPos, []) prog
    in path
evalProgram _ _ = []

compile :: Commands -> Program
compile = Map.fromList . zip [1..]


checkPath :: Field -> Path -> Bool
checkPath _ [] = False
checkPath f path = 


testingProgram :: Program
testingProgram = compile [u, u, r, r, u, l, u]

{-
solve :: Field -> Int -> Dislocation -> SolutionTree
solve f 1 d@(tink, kiki) | tink == kiki = SL d
                         | otherwise = L d
solve f ttl d@(tink, kiki) | tink == kiki = SL d
                           | otherwise = B d $ map (solve f newTtl) nextDislocations
  where
    newTtl = ttl - 1
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

-- ѕоказать статистику решений дл€ глубины n
try f n = let ss = foldSuccesses (solve f n startPositions)
              pathLengths = map length ss
              groupedPathLengths = group . sort $ pathLengths
              pathLengthStats = map (\gpls -> (head gpls, length gpls)) groupedPathLengths
          in (length ss, pathLengthStats)

-- ѕоказать решени€ длиной k дл€ глубины n
solutions f k n = let ss = foldSuccesses (solve f n startPositions)
                      kLengths = filter ((== k) . length) ss
                  in kLengths

goodSolution = solutions field1 6 10

-}