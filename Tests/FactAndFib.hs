module Main where

import Data.Word

fib :: Word -> Word
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fact :: Word -> Word
fact 0 = 1
fact n = fact (n-1) * n