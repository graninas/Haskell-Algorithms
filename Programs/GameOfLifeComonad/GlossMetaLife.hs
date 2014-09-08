module Main where

import Universe
import MetaLife

import Graphics.Gloss
import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies

import Data.Monoid

configFile = "config.txt"
screenXShift = (-30)
screenYShift = (-30)

side = 15.0
thickness = 0.5
aliveRadius = 5.0
deadRadius = 2.0

pickColor 0    = black
pickColor (-1) = red
pickColor 1    = blue
pickColor _    = green

windowCfg = InWindow "Cellular automata" (1024, 768) (500, 300)

pickCellRadius c | c == dead = deadRadius
                 | otherwise = aliveRadius

pickFactorRadius f = 7.0 + (fromIntegral f)

renderCell'' (f'', (f', c)) = Pictures [
    Color black $ Circle (pickFactorRadius f''),
    Color (pickColor f')  $ ThickCircle thickness (pickCellRadius c)
    ]
   

renderLife :: Universe2 MetaMetaCell -> Picture
renderLife = toPicture . toList2

toPicture :: [[MetaMetaCell]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [screenXShift..] picss)
  where
    toPicture' (j, pics) = map (transCellX j) (zip [screenYShift..] pics)

transCellX :: Int -> (Int, MetaMetaCell) -> Picture
transCellX j (i, mc) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (renderCell'' mc)


stepLife'' :: MetaFactor -> a -> Float -> Universe2 MetaMetaCell -> Universe2 MetaMetaCell
stepLife'' mf _ _ = stepLifeUniverse'' mf

type Universes = (Int, Int, [Universe2 MetaMetaCell])
stepUniverses :: a -> Float -> Universes -> Universes
stepUniverses _ _ (cur, end, us) | cur >= end = (cur, end, us)
                                 | otherwise  = (cur + 1, end, us)

renderUniverses :: Universes -> Picture
renderUniverses (cur, _, us) = renderLife . head . drop cur $ us

main = do
    confData <- readFile configFile
    let conf = filter (\l -> head l /= '#') . filter (not . null) . lines $ confData
    if null conf
        then putStrLn $ "Invalid configuration file: " ++ configFile
        else do
            let [f'', f', sizeStr, cntInSec] = words . head $ conf
            let mf = (read f'', read f') :: (Int, Int)
            let s = (read sizeStr) :: Int
            print $ "Config will be: " ++ show mf
            print $ "Size will be: " ++ sizeStr
            
            let m = initialModel s
            display windowCfg white (renderLife m)
            
            {-
            simulate 
                white
                (read cntInSec)
                initialModel
                renderLife''
                (stepLife'' mf)
            -}
            {-
            let universes = iterate (stepLifeUniverse'' mf) (initialModel s)
            simulate (InWindow "Cellular automata" (1024, 768) (500, 300))
                white
                (read cntInSec)
                (0, 30, universes)
                renderUniverses
                stepUniverses
            -}

