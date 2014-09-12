module SampleCells where

import MetaLife

metaCells = map (map zeroCellCreator) cells'''

cells' = [[alive, alive, dead, alive, alive]]

cells'' = [[alive, alive, alive]]

cellsGlider = [ [ dead, alive,  dead]
              , [alive,  dead,  dead]
              , [alive, alive, alive] ]

cells''' = [ [dead, dead, dead, dead, alive, dead ]
        , [dead, dead, dead, alive, dead, dead ]
        , [dead, dead, dead, alive, alive, alive ]
        , [dead, dead, dead, dead, dead, dead ]
        , [dead, dead, dead, dead, dead, dead ]
        , [dead, dead, dead, dead, dead, dead ]
        , [dead, dead, dead, dead, dead, dead ]
        , [dead, dead, dead, dead, dead, dead ]
        , [alive, dead, alive, alive, alive, alive ]
        , [dead, alive, alive, alive, dead, alive ]
        , [alive, dead, dead, alive, alive, alive ]
        , [dead, alive, dead, dead, alive, dead ]
        , [alive, dead, dead, alive, alive, dead ]
        , [dead, alive, alive, dead, dead, alive ]
        , [alive, dead, alive, alive, alive, alive ]
        , [dead, alive, alive, dead, alive, alive ]
        , [alive, dead, alive, dead, alive, alive ]
        , [dead, alive, dead, dead, alive, dead ]
        , [alive, dead, alive, alive, dead, dead ]
        , [dead, alive, alive, dead, alive, alive ]
        , [alive, dead, dead, alive, dead, dead ]
        ]

testLine  = [ alive : replicate 28 dead ++ [alive]]
testBlock = [ replicate 14 dead ++ [alive, alive] ++ replicate 14 dead]
fillers n = replicate n (replicate 30 dead)

testCells =  testLine
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testBlock
      ++ testBlock
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testLine
