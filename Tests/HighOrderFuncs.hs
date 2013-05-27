module Main where


numberList = [1, 2, 3, 4, 5]

modifyList func list = map func list

square x = x * x
double x = 2 * x
zero x = 0

squares = modifyList square numberList
doubles = modifyList double numberList
zeros = modifyList zero numberList