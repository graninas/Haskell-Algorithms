module Type.Universe where

import qualified Data.Word as W

class UniverseObject o where
	objectId :: Word32