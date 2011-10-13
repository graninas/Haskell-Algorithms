module Type.Event where

import Type.Types
import Type.Universe

import qualified Data.Foldable as F
import qualified Data.Monoid as M

data UniverseObject o => Event o = Event Position o
	deriving (Show)





{-

instance Functor (Entry k) where
	fmap f (Entry k v) = Entry k (f v)

instance Foldable (Entry k) where
	foldMap f (Entry _ v) = f v

instance Ord k => Monoid (Prio k v) where
	mempty			= NoPrio
	x `mappend` NoPrio	= x
	NoPrio `mappend` y	= y
	x@(Prio kx _) `mappend` y@(Prio ky _)
	  | kx <= ky		= x
	  | otherwise		= y

instance Ord k => Measured (Prio k v) (Entry k v) where
	measure (Entry k v) = Prio k v

-}