
module Main where

import qualified Data.Set as Set

type Node = Int
type Nodes = [Node]
type TableGraph = [ (Node, Nodes) ]

type Successors = Set.Set Node
type Graph = Node -> Successors

tarjanAlg :: Graph -> [Nodes]
tarjanAlg = undefined -- some impl

getNodes n tg = maybe [] id (lookup n tg)

asGraph :: TableGraph -> Graph
asGraph tg node = Set.fromList succs
  where
    succs = getNodes node tg

asGraph' :: TableGraph -> Graph
asGraph' tg = Set.fromList . flip getNodes tg


adaptedTarjanAlg :: TableGraph -> [Nodes]
adaptedTarjanAlg = tarjanAlg . asGraph


tableGraph :: TableGraph
tableGraph = [ (1, [2, 3])
            , (2, [])
            , (3, [1, 2]) ]
            
cycles = tarjanAlg (asGraph tableGraph)

cycles' = adaptedTarjanAlg tableGraph
