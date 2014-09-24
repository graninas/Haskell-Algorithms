
module World.Plasma where

data Plasma = Plasma { plasmaPlayer :: Player }


module World.Karyon where

data Karyon = Karyon { karyonPlayer :: Player
                     , karyonEnergy :: Energy }
                     


class Active i where
    activate :: i -> Point -> World -> OperatedWorld -> OperatedWorld
    

-- module World.Karyon:
instance Active Karyon where
    activate = undefined


-- module World.Plasma:
instance Active Plasma where
  activate = undefined
  
  
-- module GameLogic.Types:
{-# LANGUAGE ExistentialQuantification #-}
data ActiveItem = forall i. Active i => MkActiveItem i
type World = M.Map Point ActiveItem

instance Active ActiveItem where
    activate (MkActiveItem i) = activate i
    
worldMapFromList :: [(Point, ActiveItem)] -> World
worldMapFromList = M.fromList


packItem :: Active i => i -> ActiveItem
packItem = MkActiveItem

packedKaryon = packItem (Karyon 1 100)
packedPlasma = packItem (Plasma 1)

world = M.fromList [ (Point 1 1 1, packedKaryon)
                   , (Point 1 1 2, packedPlasma) ]

stepWorld world = M.foldrWithKey f M.empty world
  where
    f point (MkActiveItem i) operatedWorld = activate i point world operatedWorld
    
stepWorld = M.foldrWithKey activate M.empty

