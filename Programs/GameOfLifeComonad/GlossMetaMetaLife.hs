module Main where

import Universe
import MetaMetaLife

import Control.Comonad
import Control.Applicative
import Graphics.Gloss
import Data.Monoid

configFile = "config.txt"

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


stepLife'' :: MetaFactor -> a -> Float -> Universe2 MetaMetaCell -> Universe2 MetaMetaCell
stepLife'' mf _ _ = stepLifeUniverse'' mf

main = do
    confData <- readFile configFile
    let conf = filter (\l -> head l /= '#') . filter (not . null) . lines $ confData
    if null conf
        then putStrLn $ "Please, give right configuration in file " ++ configFile
        else do
            print $ "Got: " ++ show conf
            let [f'', f', cntInSec] = words . head $ conf
            let mf = (read f'', read f')
            print $ "Config will be: " ++ show mf
            simulate (InWindow "Cellular automata" (1024, 768) (500, 300))
                white
                (read cntInSec)
                initialModel''
                renderLife''
                (stepLife'' mf)

