{-# LANGUAGE TypeFamilies #-}

data Z 
data S n = S {sArgument :: n}
type family Plus a b
type instance Plus Z a = a
type instance Plus (S a) b = S (Plus a b)

type ONE = S Z
type TWO = Plus (S Z) (S Z)


class NatInt a
	where natInt :: a -> Int

instance NatInt Z where
	natInt = const 0
instance NatInt n => NatInt (S n) where
	natInt (S n) = 1 + natInt n