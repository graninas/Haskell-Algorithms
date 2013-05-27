module MyLibrary.MyMap where

import Data.List
import qualified Data.Map as Map
import Prelude hiding (map, (++))

myMap = Map.fromList
           [ ("ABC", 1)
           , ("BCD", 2)
           , ("CDE", 3) ]