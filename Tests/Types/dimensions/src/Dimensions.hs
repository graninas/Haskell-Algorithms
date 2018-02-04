{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Dimensions where

-- https://www.stephanboyer.com/post/131/type-safe-dimensional-analysis-in-haskell

newtype BaseQuantity a = BaseQuantity Double

-- a / b
type Quotient a b = b -> a
type Dimensionless = BaseQuantity ()
-- 1 / a
type Inverse a = Quotient Dimensionless a
-- a * b  ==  a  /  b^-1
type Product a b = Quotient a (Inverse b)

type Square a = Product a a

--
class Quantity a where
  construct :: Double -> a
  destruct  :: a -> Double

--
instance Quantity (BaseQuantity a) where
  construct = BaseQuantity
  destruct (BaseQuantity x) = x

--
instance (Quantity q, Quantity r) => Quantity (Quotient r q) where
  construct x = \y -> construct (x * (destruct y))
  destruct x = destruct (x (construct 1))

-- a / (b / c) = c / (b / a)
quotientAxiom :: (Quantity a, Quantity b, Quantity c) =>
  Quotient a (Quotient b c) -> Quotient c (Quotient b a)
quotientAxiom = construct . destruct


----  Inverse b -> a == a / b^-1  == a * b

class Productable a b where
  (.*.) :: a -> b -> Product a b

instance (Quantity a, Quantity b) => Productable a b where
  (.*.) = mulQ

instance (Quantity a, Quantity b, Quantity c) => Productable (Quotient a b) c where
  (.*.) a b = quotientAxiom (b ./. a) id

-- We can add two quantities of the same unit.
infixl 6 .+.
(.+.) :: Quantity a => a -> a -> a
(.+.) x y = construct $ (destruct x) + (destruct y)

-- We can subtract two quantities of the same unit.
infixl 6 .-.
(.-.) :: Quantity a => a -> a -> a
(.-.) x y = construct $ (destruct x) - (destruct y)

-- We can multiply any two quantities.
infixl 7 .*.
mulQ :: (Quantity a, Quantity b) => a -> b -> Product a b
mulQ x y = \z -> construct $ destruct (z y) * destruct x

-- We can divide any two quantities.
infixl 7 ./.
(./.) :: (Quantity a, Quantity b) => a -> b -> Quotient a b
(./.) x y = \z -> construct $ (destruct z) * (destruct x) / (destruct y)
