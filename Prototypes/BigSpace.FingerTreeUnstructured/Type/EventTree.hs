module Type.EventTree where

import Types.Type
import Types.Event

import qualified Data.FingerTree as FT
import qualified Data.Foldable (Foldable(foldMap)) as F

type TopHistoryMeasure = (SpaceScale, SpaceTime)
