

fact 0 = 1
fact n = fact (n-1) * n

fact' n | n == 0 = 1
        | otherwise = fact' (n-1) * n