{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionGraphSpec where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Test.Hspec

import           Lib

travel3Graph :: Graph IO () ()
travel3Graph = graph $
  with (print "3")
    <~> on "forward" (leaf (return ()))

travel2Graph :: Graph IO () ()
travel2Graph = graph $
  with (print "2")
    <~> on "forward" travel3Graph

travel1Graph :: Graph IO () ()
travel1Graph = graph $
  with (print "1")
    <~> on "forward" travel2Graph

ioRunner :: IO output -> IO (Event, output)
ioRunner act = act >>= \o -> pure ("forward", o)

spec = describe "Graph transitions test." $
  it "Test Graph transitions." $
    runGraph (GraphRuntime ioRunner (== "back")) travel1Graph
