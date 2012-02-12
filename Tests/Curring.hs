


apply x y = x y

curringTest = apply show

main = do
    putStrLn (curringTest 1)

mul x = x * 2