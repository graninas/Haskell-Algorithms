module Main where

import Universe
import MetaMetaLife

import Control.Comonad
import Control.Applicative
import Graphics.Gloss
import Data.Monoid

side = 15.0
thickness = 0.5
aliveRadius = 5.0
deadRadius = 2.0

pickColor 0    = black
pickColor (-1) = red
pickColor 1    = blue
pickColor _    = green

pickRadius f = 7.0 + (fromIntegral f)

renderCell'' (f'', (f', Alive)) = Pictures [
    Color black $ Circle (pickRadius f''),
    Color (pickColor f')  $ ThickCircle thickness aliveRadius
    ]
renderCell'' (f'', (f', Dead))  = Pictures  [
    Color black $ Circle (pickRadius f''),
    Color (pickColor f') $ ThickCircle thickness deadRadius
    ]
   

renderLife'' :: Universe2 MetaMetaCell -> Picture
renderLife'' = toPicture . takeRange2 (-30, -30) (20, 20)

toPicture :: [[MetaMetaCell]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [(-30)..] picss)
  where
    toPicture' (j, pics) = map (transCellX j) (zip [(-30)..] pics)

transCellX :: Int -> (Int, MetaMetaCell) -> Picture
transCellX j (i, mc) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (renderCell'' mc)


stepLife'' :: a -> Float -> Universe2 MetaMetaCell -> Universe2 MetaMetaCell
stepLife'' _ _ = stepLifeUniverse''

main = simulate (InWindow "Cellular automata" (1024, 768) (500, 300))
                white
                3
                initialModel''
                renderLife''
                stepLife''

