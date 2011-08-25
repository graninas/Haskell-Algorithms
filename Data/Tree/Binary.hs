module Data.Tree.Binary where

data Ord a => BTree a b =
						  Leaf
						| Branch a b (BTree a b) (BTree a b)
	deriving (Show)


leaf = Leaf
branch = Branch
left  (Branch _ _ l _) = l
right (Branch _ _ _ r) = r

lookup :: Ord a => a -> BTree a b -> Maybe b
lookup _ Leaf = Nothing
lookup key (Branch k v ltree rtree) | key == k  = Just v
									| key < k   = lookup key ltree
									| otherwise = lookup key rtree

insert :: Ord a => (a, b) -> BTree a b -> BTree a b
insert (key, val) Leaf = Branch key val Leaf Leaf
insert p@(key, val) (Branch k v ltree rtree) | key == k =  Branch key val ltree rtree
									 | key < k =   Branch k v (insert p ltree) rtree
									 | otherwise = Branch k v ltree (insert p rtree)

minBranch :: Ord a => BTree a b -> BTree a b
minBranch m@(Branch k v Leaf _) = m
minBranch (Branch _ _ l@(Branch _ _ _ _) _) = minBranch l
minBranch _ = Leaf

delete :: Ord a => a -> BTree a b -> BTree a b
delete _ Leaf = Leaf
delete key (Branch k v ltree Leaf)  | key == k = ltree
delete key (Branch k v ltree rtree) | key == k = let mb@(Branch mk mv _ _) = minBranch rtree
											  in Branch mk mv ltree (delete mk rtree)
								 | key < k = Branch k v (delete key ltree) rtree
								 | key > k = Branch k v ltree (delete key ltree)

fromList :: Ord a => [(a, b)] -> BTree a b
fromList = foldr insert Leaf

toList :: Ord key => BTree key value -> [(key, value)]
toList Leaf = []
toList (Branch k v ltree rtree) = ((k, v) : toList ltree) ++ toList rtree