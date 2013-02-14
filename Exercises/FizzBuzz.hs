{-
Напишите программу, которая выводит на экран числа от 1 до 100. При этом вместо чисел, кратных трем, программа должна выводить слово «Fizz», а вместо чисел, кратных пяти — слово «Buzz». Если число кратно и 3, и 5, то программа должна выводить слово «FizzBuzz»
-}


getOutput :: Int -> String
getOutput x | isDivided x 15 = "FizzBuzz"
			| isDivided x 5  = "Buzz"
			| isDivided x 3  = "Fizz"
			| otherwise      = show x
	where
		isDivided x n = (x `mod` n) == 0

main = do
		mapM_ (putStrLn . getOutput) [1..100]