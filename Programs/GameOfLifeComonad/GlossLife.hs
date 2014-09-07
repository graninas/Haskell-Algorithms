module Main where

import Universe
import Life

import Control.Comonad
import Control.Applicative
import Graphics.Gloss
import Data.Monoid

side = 15.0
renderCell Alive = Circle 5.0
renderCell Dead = Circle 2.0

renderLife :: Universe2 Cell -> Picture
renderLife = toPicture . takeRange2 (-7, -7) (20, 20)

toPicture :: [[Cell]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [0..] picss)
  where
    toPicture' (j, pics) = map (transCellX j) (zip [0..] pics)

transCellX :: Int -> (Int, Cell) -> Picture
transCellX j (i, c) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (renderCell c)


stepLife :: a -> Float -> Universe2 Cell -> Universe2 Cell
stepLife _ _ = (=>> rule)

main = simulate (InWindow "Cellular automata" (1024, 768) (500, 300))
                white
                1
                initialModel
                renderLife
                stepLife

