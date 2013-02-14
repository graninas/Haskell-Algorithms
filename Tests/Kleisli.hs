import Data.Char
import Data.List

-- test' :: String -> IO ()
-- test' = runKleisli (Klesli purStrLn . arr (map toUpper) . Kleisli readFile)

test :: String -> IO ()
test = (putStrLn =<<) . fmap (map toUpper) . readFile

test2 = fmap (putStrLn . map toUpper) . readFile

main = test2 "LICENSE"