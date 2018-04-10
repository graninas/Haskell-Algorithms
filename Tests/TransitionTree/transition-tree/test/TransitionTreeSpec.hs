{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionTreeSpec where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Test.Hspec

import           Lib

printLevel :: String -> Lang ()
printLevel = printS

travel3Tree :: Tree () ()
travel3Tree = tree $
  with (printLevel "3")
    <~> on "forward" (leaf (return ()))

travel2Tree :: Tree () ()
travel2Tree = tree $
  with (printLevel "2")
    <~> on "forward" travel3Tree

travel1Tree :: Tree () ()
travel1Tree = tree $
  with (printLevel "1")
    <~> on "forward" travel2Tree

interpretLang :: LangF s -> IO s
interpretLang (PrintS s next)  = print s >> return next
interpretLang (GetInput nextF) = error "Not implemented."

runLang :: Lang s -> IO (Event, s)
runLang l = do
  r <- foldFree interpretLang l
  return ("forward", r)

isBkEv event = event == "back"

spec = describe "Tree transitions test." $
  it "Test Tree transitions." $
    runTree (Runtime runLang isBkEv) travel1Tree
