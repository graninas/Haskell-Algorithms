module Main where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS

import           Lib
import           AdvGameLang

type AGGraph a b = Graph AdventureL a b

printLevel :: String -> AdventureL ()
printLevel = printS

nop :: AdventureL ()
nop = pure ()

travel3Graph :: AGGraph () ()
travel3Graph = graph $
  with location3
    <~> on "forward" (leaf nop)

travel2Graph :: AGGraph () ()
travel2Graph = graph $
  with location2
    <~> on "forward" travel3Graph

travel1Graph :: AGGraph () ()
travel1Graph = graph $
  with location1
    <~> on "forward" travel2Graph

location1 :: AdventureL ()
location1 = location "You are standing on front of a house."

location2 :: AdventureL ()
location2 = location "Another location."

location3 :: AdventureL ()
location3 = location "Location #3."

location :: String -> AdventureL ()
location description = do
  printLevel description

interpret :: AdventureLF s -> IO s
interpret (PrintS s next)  = print s >> pure next
interpret (GetInput nextF) = error "Not implemented."

run :: AdventureL s -> IO (Event, s)
run l = do
  result <- foldFree interpret l
  input  <- getLine
  pure (input, result)

isBkEv event = event == "back"


main :: IO ()
main = do
  runGraph (Runtime run isBkEv) travel1Graph


  pure ()
