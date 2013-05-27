

sumList [] = 0
sumList (x:xs) = x + sumList xs

sumList' xs = foldr (+) 0 xs

