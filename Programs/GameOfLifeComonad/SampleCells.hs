module SampleCells where

import Universe
import MetaLife

metaCells = map (map zeroCellCreator) cells'''

cells' = [[alive, alive, dead, alive, alive]]

cells'' = [[alive, alive, alive]]

cells = [ [ dead, alive,  dead]
        , [alive,  dead,  dead]
        , [alive, alive, alive] ]

cells''' = [ [alive, dead, alive, alive, alive, alive ]
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