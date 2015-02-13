{-# LANGUAGE DeriveFunctor #-}
module FractalComonad where

import Control.Comonad
import Control.Applicative

data Layer a = Layer a
  deriving (Show, Read, Eq, Functor)

instance Comonad Layer where
--  duplicate :: w a -> w (w a)
    duplicate (Layer l) = Layer (Layer l)
--    extract :: w a -> a
    extract (Layer l) = l


test = mkCantor 0.0 1.0
checkComLaws1 = (extract . duplicate $ test) == test
checkComLaws2 = (fmap extract . duplicate $ test) == test
checkComLaws3 = (duplicate . duplicate $ test) == (fmap duplicate . duplicate $ test)
cLaws = [checkComLaws1, checkComLaws2, checkComLaws3]


type Segments = [(Float, Float)]

cantorRule :: Layer Segments -> Segments
cantorRule sc = let
    fracts = extract sc
    f (x1, x2) = let
        l = x2 - x1
        l3 = l / 3.0
        in [(x1, x1 + l3), (x2 - l3, x2)]
    in concatMap f fracts
    
mkCantor :: Float -> Float -> Layer Segments
mkCantor x1 x2 = Layer [(x1, x2)]

cantorGen = (=>> cantorRule)

fractal = iterate cantorGen test

