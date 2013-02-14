

data Tree = Leaf (Int, Int) | Branch (Int, Int) Tree Tree
	deriving (Show)



leftBranch = \(cl, cr) ->
		let
			ll = cl
			lr = cl + (cr - cl) `div` 2
		in (ll, lr)

rightBranch = \(cl, cr) ->
		let
			rl = cl + (cr - cl) `div` 2 + 1
			rr = cr
		in (rl, rr)



mkTree :: Int -> (Int, Int) -> Tree
mkTree 0 cur = let
					lb = leftBranch cur
					rb = rightBranch cur
				in Branch cur (Leaf lb) (Leaf rb)
mkTree n cur = let
					lb = leftBranch cur
					rb = rightBranch cur
				in Branch cur (mkTree (n-1) lb) (mkTree (n-1) rb)