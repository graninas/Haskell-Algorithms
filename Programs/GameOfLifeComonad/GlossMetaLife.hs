module Main where

import Universe
import MetaLife

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

renderMetaCell (i, Alive) = Color (pickColor i) $ ThickCircle thickness aliveRadius
renderMetaCell (i, Dead)  = Color (pickColor i) $ ThickCircle thickness deadRadius

renderMetaLife :: Universe2 MetaCell -> Picture
renderMetaLife = toPicture . takeRange2 (-15, -15) (20, 20)

toPicture :: [[MetaCell]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [(-10)..] picss)
  where
    toPicture' (j, pics) = map (transCellX j) (zip [(-10)..] pics)

transCellX :: Int -> (Int, MetaCell) -> Picture
transCellX j (i, mc) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (renderMetaCell mc)


stepMetaLife :: a -> Float -> Universe2 MetaCell -> Universe2 MetaCell
stepMetaLife _ _ = stepMetaLifeUniverse

main = simulate (InWindow "Cellular automata" (1024, 768) (500, 300))
                white
                2
                initialMetaModel
                renderMetaLife
                stepMetaLife

