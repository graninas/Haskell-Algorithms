module Type.Types where

import qualified Data.Word as W

import Type.Constants

data GeometryPosition = GP
	{
		gpLocal1Scale :: GeometryScale,
		gpLocal2Scale :: GeometryScale,
		gpPlanetaryScale :: GeometryScale,
		gpStarSystemScale :: GeometryScale
	}
	deriving (Show)

data TimePosition = TP
	{
		tpLocalScale :: TimeScale,
		tpAgeScale :: TimeScale,
		tpEpochScale :: TimeScale
	}
	deriving (Show)

data Position = Pos
	{
		pGeometryPosition :: GeometryPosition,
		pTimePosition :: TimePosition
	}
	deriving (Show)

type SpaceScale = W.Word64
type SpaceTime = W.Word64


