

s = map (\x -> 1 / x^2) [1, 2..]

s' = sum (take 1000000 s)