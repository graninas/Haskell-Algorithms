module RandomTreap where

import qualified Treap as T
import qualified System.Random as R

newtype RTreap g k p v = RT (g, T.Treap k p v)
	deriving Show

insert :: (R.RandomGen g, Ord a, Enum a, Ord b, Num b, R.Random b) => T.RecalcFunc a b c -> (a, c) -> RTreap g a b c -> RTreap g a b c
insert f (k, v) (RT (g, tree)) = let (p, g') = R.randomR (-1000, 1000) g
								 in RT (g', T.insert f (k, p, v) tree)

delete :: (Ord a, Enum a, Ord b) => a -> T.RecalcFunc a b c -> RTreap g a b c -> RTreap g a b c
delete _ _ (RT (g, T.Leaf)) = RT (g, T.Leaf)
delete x0 f (RT (g, tree)) = RT (g, T.delete x0 f tree)

fromList :: (R.RandomGen g, Ord a, Enum a, Ord b, Num b, R.Random b) => g -> T.RecalcFunc a b c -> [(a, c)] -> RTreap g a b c
fromList g f = foldr (insert f) (RT (g, T.Leaf))

toList :: (Ord a, Enum a, Ord b) => RTreap g a b c -> [(a, c)]
toList (RT (_, tree)) = [(k, v) | (k, _, v) <- T.toList tree]

toSortedListL :: (Ord a, Enum a, Ord b) => RTreap g a b c -> [(a, c)]
toSortedListL (RT (_, tree)) = [(k, v) | (k, _, v) <- T.toSortedListL tree]

toSortedListR :: (Ord a, Enum a, Ord b) => RTreap g a b c -> [(a, c)]
toSortedListR (RT (_, tree)) = [(k, v) | (k, _, v) <- T.toSortedListR tree]

val :: (Ord a, Enum a, Ord b) => T.Treap a b Int -> Int
val T.Leaf = 0
val (T.Branch _ _ v _ _) = v

recalc :: (Ord a, Enum a, Ord b) => T.RecalcFunc a b Int
recalc l r = val l + val r

testData :: [(Int, Int)]
testData = [(1, 2), (3, 6), (6, 44), (7, 0), (2, 85), (4, -2), (5, 4), (8, 21)]