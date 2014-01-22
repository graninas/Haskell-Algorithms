module Main where

import qualified Data.Map as M

phonebook :: [(String, String)]
phonebook =
        [ ("Bob",   "01788 665242")
        , ("Fred",  "01624 556442")
        , ("Alice", "01889 985333")
        , ("Jane",  "01732 187565") ]

type PhoneMap = M.Map String String

phoneMap :: PhoneMap
phoneMap = M.fromList phonebook


getPhone :: String -> Maybe String
getPhone name = lookup name phonebook

printPhone :: Maybe String -> IO ()
printPhone Nothing = putStrLn "Not found."
printPhone (Just phone) = putStrLn phone

evalPhone :: String -> IO ()
evalPhone name = (printPhone . getPhone) name
showPhone :: String -> IO ()

showPhone name = printPhone $ getPhone name

addPhone n p = M.insert n p phoneMap
updatePhone n p = M.update (\_ -> Just p) n phoneMap

main' = printPhone . getPhone $ "Alice"
main'' = printPhone (getPhone "Alice")
main''2 = printPhone $ getPhone "Alice"
main''3 = printPhone $ getPhone $ "Alice"

main''' = do
    putStr "Enter name: "
    name <- getLine
    printPhone (getPhone name)


main'''' = do
    putStr "Enter name: "
    name <- getLine
    let phone = getPhone name
    printPhone phone