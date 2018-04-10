module Main where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS

import           Lib



printLevel :: String -> Lang ()
printLevel = printS

travel3Tree :: Tree () ()
travel3Tree = tree $
  with location3
    <~> on "forward" (leaf (return ()))

travel2Tree :: Tree () ()
travel2Tree = tree $
  with location2
    <~> on "forward" travel3Tree

travel1Tree :: Tree () ()
travel1Tree = tree $
  with location1
    <~> on "forward" travel2Tree

location1 :: Lang ()
location1 = location "You are standing on front of a house."

location2 :: Lang ()
location2 = location "Another location."

location3 :: Lang ()
location3 = location "Location #3."

location :: String -> Lang ()
location description = do
  printLevel description
  -- getInput

interpretLang :: LangF s -> IO s
interpretLang (PrintS s next)  = print s >> return next
-- interpretLang (GetInput nextF) = nextF <$> getLine
interpretLang (GetInput nextF) = error "Not implemented."

runLang :: Lang s -> IO (Event, s)
runLang l = do
  result <- foldFree interpretLang l
  input  <- getLine
  return (input, result)

isBkEv event = event == "back"


main :: IO ()
main = do
  runTree (Runtime runLang isBkEv) travel1Tree


  pure ()
