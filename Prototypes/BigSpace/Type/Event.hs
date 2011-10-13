module Type.Event where

import Type.Types
import qualified Data.Foldable as F
import qualified Data.Monoid as M

data Event evtype = Event evtype
	deriving (Show)

class ComposableEvent




instance Functor Event where
	fmap f (Event evtype) = Event (f evtype)

instance F.Foldable Event where
	foldMap f (Event evtype) = f evtype


data EventMeasure evtype
						= EmptyEvent
						| EventMeasure evtype

instance ComposableEvent evtype => M.Monoid (EventMeasure evtype) where
	mempty = EmptyEvent
	x `mappend` EmptyEvent = x
	EmptyEvent `mappend` y = y
	(EventMeasure ev1) `mappend` (EventMeasure ev2) = EventMeasure (composeEvents ev1 ev2)


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