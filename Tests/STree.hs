module Main where

data STree a
           = Tip
           |  Node (STree a) a (STree a)
  deriving (Eq, Show, Read)

main = do
    let strTree = show myTree
    writeFile "tree.txt" strTree
    fileContent <- readFile "tree.txt"
    let readedTree = read fileContent
    print . height $ readedTree

type IntSTree = STree Int

myTree, left, right :: IntSTree

myTree = Node left 5 right


left   = Node subLeft 3 subRight
right = Node Tip 7 Tip

subLeft, subRight :: IntSTree
subLeft   = Node Tip 1 Tip
subRight = Node Tip 4 Tip

height :: IntSTree -> Int
height Tip = 0
height (Node left _ right) = 1 + subHeight
  where
    subHeight = max (height left) (height right)

compare :: STree Int -> STree Int -> Bool
compare t1 t2 = t1 == t2

nodeWeight :: IntSTree -> (Int, Int)
nodeWeight Tip = (0, 0)
nodeWeight (Node l val r) = let
    (_, lWeight) = nodeWeight l
    (_, rWeight) = nodeWeight r
    in (val, val + lWeight + rWeight)

class  Show a  where
    show :: a -> String

class Read a where
  readsPrec :: Int -> ReadS a
  readList     :: ReadS [a]
  readPrec     :: ReadPrec a
  readListPrec :: ReadPrec [a]

read :: Read a => String -> a