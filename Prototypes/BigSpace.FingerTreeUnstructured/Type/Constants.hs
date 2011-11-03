module Type.Constants where

import qualified Data.Word as W

type GeometryScale = W.Word32
type TimeScale = W.Word32

geometryLocalScale1Multiplier :: GeometryScale
geometryLocalScale1Multiplier = 1000000

geometryLocalScale2Multiplier :: GeometryScale
geometryLocalScale2Multiplier = 1000
