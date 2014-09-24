module GameLogic.Types where
import qualified Data.Map as M

data Item = Karyon ...
          | Plasma ...
          | Mitochondrion ...
          | Stone ...

type World = M.Map Point Item
type OperatedWorld = World

module GameLogic.Logic where
import GameLogic.Types

stepWorld :: World -> World
apply :: World -> Item -> OperatedWorld -> OperatedWorld


import GameLogic.Karyon as Karyon
import GameLogic.Plasma as Plasma

apply w (Karyon a b c) ow = Karyon.apply w (a, b, c) ow
apply w (Plasma c d) ow = Plasma.apply w (c, d) ow
apply w (Mitochondrion e f g) ow = Mitochondrion.apply w (e, f, g) ow
...

getEnergy :: Item -> Int
getEnergy (Karyon e b c) = e
getEnergy (Mitochondrion e f g) = e
getEnergy _ = error "getEnergy unsupported."


main = do
    putStrLn "Ok."