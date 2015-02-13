{-# LANGUAGE DeriveFunctor #-}
module FractalComonad where

import Control.Comonad
import Control.Applicative

data Fractal a = Fractal a (Maybe (Fractal a))
  deriving (Show, Read, Eq, Functor)

instance Comonad Fractal where
--  duplicate :: w a -> w (w a)
    --duplicate (Fractal frs) = Fractal (Fractal frs)
    --duplicate (Fractal frs Nothing) = Fractal (Fractal frs Nothing) (Just (Fractal (Fractal frs Nothing) Nothing))
    duplicate (Fractal frs Nothing) = Fractal (Fractal frs Nothing) Nothing
    duplicate (Fractal frs mb) = Fractal (Fractal frs Nothing) (Just (Fractal (Fractal frs mb) Nothing))
--    extract :: w a -> a
    extract (Fractal frs Nothing) = frs
    extract (Fractal frs (Just (Fractal frs2 mb))) = frs2


lawTest = mkCantor 0.0 1.0
a = extract . duplicate $ lawTest
b = fmap extract . duplicate $ lawTest
c = duplicate . duplicate $ lawTest
d = duplicate $ lawTest
checkComLaws1 = (extract . duplicate $ lawTest) == lawTest
checkComLaws2 = (fmap extract . duplicate $ lawTest) == lawTest
checkComLaws3 = (duplicate . duplicate $ lawTest) == (fmap duplicate . duplicate $ lawTest)
cLaws = [checkComLaws1, checkComLaws2, checkComLaws3]

type Segment = (Float, Float)
data F a = Single a
         | Multiple [a]
   deriving (Show, Read, Eq)
         
cantorRule :: Fractal [Segment] -> [Segment]
cantorRule fr = let
    newGen xs = do
        (x1, x2) <- xs
        let l = x2 - x1
        let l3 = l / 3.0
        y <- [(x1, x1 + l3), (x2 - l3, x2)]
        return y
    oldXs = extract fr
    in newGen oldXs
    
--mkCantor :: Float -> Float -> Fractal (F Segment)
mkCantor x1 x2 = Fractal [(x1, x2)] Nothing
cantor = mkCantor 0.0 9.0

next = (=>> cantorRule)

fractal = iterate next cantor



