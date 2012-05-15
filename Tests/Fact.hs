fact1 0 = 1
fact1 n = fact1 (n-1) * n

fact2 n | n == 0 = 1
        | otherwise = fact2 (n-1) * n

fact3 n = case n == 0 of
    True  -> 1
    False -> fact3 (n-1) * n

fact4 n = product [1..n]

fact5 n = if n == 0 
          then 1
          else n * fact5 (n-1)