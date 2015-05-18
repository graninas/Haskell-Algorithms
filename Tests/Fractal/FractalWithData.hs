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


checkComLaws1 = (extract . duplicate $ cantorLayer) == cantorLayer
checkComLaws2 = (fmap extract . duplicate $ cantorLayer) == cantorLayer
checkComLaws3 = (duplicate . duplicate $ cantorLayer) == (fmap duplicate . duplicate $ cantorLayer)
cLaws = [checkComLaws1, checkComLaws2, checkComLaws3]

comonadCantorRule :: Layer Segments -> Segments
comonadCantorRule layer = concatMap cantorRule . extract
    let segments = extract layer
    in concatMap cantorRule segments


type Segment = (Float, Float)
type Segments = [(Float, Float)]

cantorRule :: Segment -> Segments
cantorRule (x1, x2) = let
    len = x2 - x1
    oneThird = len / 3.0
    in [(x1, x1 + oneThird), (x2 - oneThird, x2)]


cantorCustomGen :: Segments -> Segments
cantorCustomGen segs = concatMap cantorRule segs

fractal' :: [Segments]
fractal' = iterate cantorCustomGen [(0.0, 0.9)]


cantorStartSegment x1 x2 = [(x1, x2)]

cantorLayer = mkCantor 0.0 9.0
mkCantor :: Float -> Float -> Layer Segments
mkCantor x1 x2 = Layer $ cantorStartSegment x1 x2

comonadCantorGen :: Layer Segments -> Layer Segments
comonadCantorGen = (=>> comonadCantorRule)

fractal = iterate comonadCantorGen cantorLayer

{- LYAH -}
{-
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)
    
data TreeZ a = L a (Tree a)
             | R a (Tree a)
    deriving (Show)  

type TreeZS a = [TreeZ a]  
type TreeZipper a = (Tree a, TreeZS a)

left, right :: TreeZipper a -> TreeZipper a
left  (Node x l r, bs) = (l, L x r:bs)
right (Node x l r, bs) = (r, R x l:bs)

type CantorTree = Tree Segment
genLSegment x = Node (head $ cantorGen x) Empty Empty
genRSegment x = Node (head $ tail $ cantorGen x) Empty Empty
cantorLeft  (Node x Empty r, bs) = (genLSegment x, L x r : bs)
cantorLeft  (Node x l r, bs)     = (l, L x r : bs)
cantorRight (Node x l Empty, bs) = (genRSegment x, R x l : bs)

-}
