{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionGraphSpec where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Test.Hspec

import           Lib

printLevel :: String -> Lang ()
printLevel = printS

travel3Graph :: Graph () ()
travel3Graph = graph $
  with (printLevel "3")
    <~> on "forward" (leaf (return ()))

travel2Graph :: Graph () ()
travel2Graph = graph $
  with (printLevel "2")
    <~> on "forward" travel3Graph

travel1Graph :: Graph () ()
travel1Graph = graph $
  with (printLevel "1")
    <~> on "forward" travel2Graph

interpretLang :: LangF s -> IO s
interpretLang (PrintS s next)  = print s >> return next
interpretLang (GetInput nextF) = error "Not implemented."

runLang :: Lang s -> IO (Event, s)
runLang l = do
  r <- foldFree interpretLang l
  return ("forward", r)

isBkEv event = event == "back"

spec = describe "Graph transitions test." $
  it "Test Graph transitions." $
    runGraph (Runtime runLang isBkEv) travel1Graph
