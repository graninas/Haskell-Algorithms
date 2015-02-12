{-# LANGUAGE DeriveFunctor #-}
module FractalComonad where

import Control.Comonad
import Control.Applicative

{-
type Factor = Int
data Fractal a = Fractal Factor a [a]
  deriving (Show, Read, Eq)

instance Functor Fractal where
--    fmap :: (a -> b) -> f a -> f b
    fmap g (Fractal i fr0 frs) = Fractal i (g fr0) (map g frs)

simple :: Factor -> a -> Fractal a
simple i a = Fractal i a []

generate :: Factor -> a -> Fractal a
generate i fr0 = Fractal i fr0 (replicate i fr0)


test = generate 3 5
law1_1 = fmap id $ test
law2_1 = fmap (+1) . fmap (*2) $ test
law2_2 = fmap ((+1) . (*2))    $ test

fLaws = [law1_1 == test, law2_1 == law2_2]


instance Comonad Fractal where
--  duplicate :: w a -> w (w a)
    duplicate (Fractal i fr0 frs) = Fractal i (generate i fr0)
                                              (map (generate i) frs)
--    extract :: w a -> a
    extract (Fractal i fr0 frs) = fr0

checkComLaws1 = (extract . duplicate $ test) == test
checkComLaws2 = (fmap extract . duplicate $ test) == test
checkComLaws3 = (duplicate . duplicate $ test) == (fmap duplicate . duplicate $ test)

cLaws = [checkComLaws1, checkComLaws2, checkComLaws3]

-}

data Fractal a = Fractal a
  deriving (Show, Read, Eq, Functor)

instance Comonad Fractal where
--  duplicate :: w a -> w (w a)
    duplicate (Fractal frs) = Fractal (Fractal frs)
--    extract :: w a -> a
    extract (Fractal frs) = frs


test = mkCantor 0.0 1.0
checkComLaws1 = (extract . duplicate $ test) == test
checkComLaws2 = (fmap extract . duplicate $ test) == test
checkComLaws3 = (duplicate . duplicate $ test) == (fmap duplicate . duplicate $ test)
cLaws = [checkComLaws1, checkComLaws2, checkComLaws3]

cantor :: Fractal [(Float, Float)] -> [(Float, Float)]
cantor fr = let
    fracts = extract fr
    f (x1, x2) = let
        l = x2 - x1
        l3 = l / 3.0
        in [(x1, x1 + l3), (x2 - l3, x2)]
    in concatMap f fracts
    
cantorStart :: Fractal [(Float, Float)]
cantorStart = Fractal [(0.0, 1.0)]
mkCantor :: Float -> Float -> Fractal [(Float, Float)]
mkCantor x1 x2 = Fractal [(x1, x2)]

next = (=>> cantor) 



