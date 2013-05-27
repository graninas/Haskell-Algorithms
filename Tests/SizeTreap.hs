module SizeTreap where

import Data.Tree.Treap

testData :: [(Int, Int, Int)]
testData = [(1, 2, 1), (3, 6, 1), (6, 44, 1), (7, 0, 1), (2, 85, 1), (4, -2, 1), (5, 4, 1), (8, 21, 1)]

sizeOf :: (Ord a, Enum a, Ord b) => Treap a b Int -> Int
sizeOf Leaf          = 0
sizeOf (Branch _ _ v _ _) = v

recalcSize :: (Ord a, Enum a, Ord b) =>  RecalcFunc a b Int
recalcSize l r = sizeOf l + sizeOf r + 1

kthElement :: (Ord a, Enum a, Ord b) => Int -> Treap a b Int -> Maybe (a, b, Int)
kthElement k Leaf = Nothing
kthElement k (Branch k1 p1 v1 l r) | sizeLeft == k = Just (k1, p1, v1)
								   | sizeLeft > k  = kthElement k l
								   | sizeLeft < k  = kthElement (k - sizeLeft - 1) r
	where sizeLeft = (sizeOf l)

fromList' :: (Ord a, Enum a, Ord b) => [(a, b, Int)] -> Treap a b Int
fromList' = foldr (insert recalcSize) Leaf