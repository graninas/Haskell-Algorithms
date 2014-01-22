{-# LANGUAGE PackageImports #-}

module Main where

import "mtl" Control.Monad.State.Lazy


myFunc :: State Int Int
myFunc = do
    val <- get
    put (val - 8)
    get

getNumber = evalState myFunc 50
main = print getNumber