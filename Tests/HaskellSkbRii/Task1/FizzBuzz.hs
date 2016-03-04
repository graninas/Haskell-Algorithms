{-
    Напишите программу, которая выводит на экран числа от 1 до 100.
    При этом вместо чисел, кратных трем, программа должна выводить
    слово «Fizz», а вместо чисел, кратных пяти — слово «Buzz».
    Если число кратно и 3, и 5, то программа должна выводить слово «FizzBuzz»
-}

module Main where

fizzBuzz :: Int -> String
fizzBuzz = undefined

main = do
    let myFizzBuzzes = --put your solution here

    fbs <- readFile "FizzBuzzes.txt"    
    if read fbs == myFizzBuzzes
        then putStrLn "Right."
        else putStrLn "Not right."
