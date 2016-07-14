module PathFind (findPath, dijkstra, aStar) where

import qualified PathFind.Dijkstra as D
import qualified PathFind.AStar as A

data FindPathAlgorithm = Dijkstra | AStar

findPath Dijkstra = D.findPath
findPath AStar = A.findPath

dijkstra = Dijkstra
aStar = AStar