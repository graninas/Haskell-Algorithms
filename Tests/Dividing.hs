{-
Напишите код, который находит количество подчисел числа n, на которые это число делится без остатка.

Для числа n, подчисло — это такое число, запись которого является подстрокой записи числа n. К примеру, если n равняется 1938, то его подчислами будут являться: 1, 9, 3, 8, 19, 93, 38, 193 и 938. Без остатка 1938 делится на четыре из этих подчисел: 1, 3, 19 и 38. Соответственно, результатом работы программы должно быть число 4.
Если подчисла повторяются, каждое из них считается. Например, 101 делится без остатка на 1, 1 и 01, значит, ответ — 3.
-}

import Data.List

dividedBy :: Int -> String -> Bool
dividedBy _ [] = False
dividedBy n b = case read b of
					0 -> False
					x -> n `mod` x == 0 && n /= x


sublists :: String -> [String]
sublists [] = []
sublists s@(c:cs) = takeSub s (length s) ++ sublists cs
    where
        takeSub _ 0 = []
        takeSub s count = take count s : takeSub s (count - 1)

dividers :: Int -> Int
dividers n = length . filter (dividedBy n) . sublists . show $ n

main = putStrLn . show . dividers $ 1938