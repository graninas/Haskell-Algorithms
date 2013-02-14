module Tools where


numberPowList :: Int -> [(Int, Int)]
numberPowList n = zip [0..] $ scanl (*) 1 (repeat n)