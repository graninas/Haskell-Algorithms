module Treap where

import Prelude hiding (lookup)

data (Ord a, Enum a, Ord b) => Treap a b c =
						  Leaf
						| Branch a b c (Treap a b c) (Treap a b c)
	deriving (Show)

type RecalcFunc a b c = Treap a b c -> Treap a b c -> c

leaf :: Treap a b c
leaf = Leaf

branch :: (Ord a, Enum a, Ord b) => a -> b -> c -> Treap a b c -> Treap a b c -> Treap a b c
branch = Branch

left  (Branch _ _ _ l _) = l
right (Branch _ _ _ _ r) = r

singleton :: (Ord a, Enum a, Ord b) => (a, b, c) -> Treap a b c
singleton (k, p, v) = Branch k p v Leaf Leaf

merge :: (Ord a, Enum a, Ord b) => RecalcFunc a b c -> Treap a b c -> Treap a b c -> Treap a b c
merge _ Leaf x = x
merge _ x Leaf = x
merge _ (Branch k1 _ _ _ _) (Branch k2 _ _ _ _) | k1 > k2 = undefined
merge f l@(Branch k1 p1 v1 ll lr) r@(Branch k2 p2 v2 rl rr) | p1 > p2  = let subTree = merge f lr r
																		 in  Branch k1 p1 (f subTree ll) ll subTree
															| p1 <= p2 = let subTree = merge f l rl
																		 in  Branch k2 p2 (f subTree rr) subTree rr

split :: (Ord a, Enum a, Ord b) => a -> RecalcFunc a b c -> Treap a b c -> (Treap a b c, Treap a b c)
split _ _ Leaf = (Leaf, Leaf)
split x0 f (Branch k p v l r) | k <= x0 = let (l', r') = split x0 f r
										  in  (Branch k p (f l l') l l', r')
							  | k > x0  = let (l', r') = split x0 f l
										  in  (l', Branch k p (f r' r) r' r)

insert :: (Ord a, Enum a, Ord b) => RecalcFunc a b c -> (a, b, c) -> Treap a b c -> Treap a b c
insert _ (x0, p, v) Leaf = Branch x0 p v Leaf Leaf
insert f (x0, p, v) tree = let (l, r) = split x0 f tree
						   in merge f (merge f l (Branch x0 p v Leaf Leaf)) r

delete :: (Ord a, Enum a, Ord b) => a -> RecalcFunc a b c -> Treap a b c -> Treap a b c
delete _ _ Leaf = Leaf
delete x0 f tree = let
					(l, r) = split (pred x0) f tree
					(l', r') = split x0 f r
				   in merge f l r'

lookup :: (Ord a, Enum a, Ord b) => a -> Treap a b c -> Maybe (a, b, c)
lookup k Leaf = Nothing
lookup k (Branch k1 p1 v1 l r) | k == k1 = Just (k1, p1, v1)
							   | k < k1 = lookup k l
							   | k > k1 = lookup k r

fromList :: (Ord a, Enum a, Ord b) =>  RecalcFunc a b c -> [(a, b, c)] -> Treap a b c
fromList f = foldr (insert f) Leaf

toList :: (Ord a, Enum a, Ord b) => Treap a b c -> [(a, b, c)]
toList Leaf = []
toList (Branch k p v l r) = ((k, p, v) : toList l) ++ toList r

toSortedListL :: (Ord a, Enum a, Ord b) => Treap a b c -> [(a, b, c)]
toSortedListL Leaf = []
toSortedListL (Branch k p v l r) = toSortedListL l ++ ((k, p, v) : toSortedListL r)

toSortedListR :: (Ord a, Enum a, Ord b) => Treap a b c -> [(a, b, c)]
toSortedListR Leaf = []
toSortedListR (Branch k p v l r) = toSortedListR r ++ ((k, p, v) : toSortedListR l)