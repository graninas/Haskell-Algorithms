module Main where

import Universe
import MetaLife

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies

import Data.Monoid

configFile = "config.txt"
screenXShift = (-15)
screenYShift = (-15)

side = 15.0
thickness = 0.5
aliveRadius = 5.0
deadRadius = 2.0

pickColor 0    = black
pickColor (-1) = red
pickColor 1    = blue
pickColor _    = green

windowCfg = InWindow "Cellular automata" (1024, 768) (500, 300)

data Mode = Play | Simulate
  deriving (Show, Read)

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


stepLife :: MetaFactor -> a -> Float -> Universe2 MetaMetaCell -> Universe2 MetaMetaCell
stepLife mf _ _ = stepLifeUniverse'' mf

type Universes = (Int, Int, [Universe2 MetaMetaCell])

stepUniverses' :: Universes -> Universes
stepUniverses' (cur, end, us) | cur >= end = (cur, end, us)
                              | otherwise  = (cur + 1, end, us)
stepUniverses :: a -> Float -> Universes -> Universes
stepUniverses _ _ = stepUniverses'

renderUniverses :: Universes -> Picture
renderUniverses (cur, _, us) = renderLife . head . drop cur $ us

eventHandler :: Event -> Universes -> Universes
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) us = stepUniverses' us
eventHandler _ us = us

mkIteration ti s mf = (0, ti, universes)
  where universes = iterate (stepLifeUniverse'' mf) (initialModel s)

run Play ti ips s mf = play windowCfg white ips
                             (mkIteration ti s mf)
                             renderUniverses
                             eventHandler
                             (const id)
run Simulate ti ips s mf = simulate windowCfg white ips
                             (mkIteration ti s mf)
                             renderUniverses
                             stepUniverses



main = do
    confData <- readFile configFile
    let conf = filter (\l -> head l /= '#') . filter (not . null) . lines $ confData
    if null conf
        then putStrLn $ "Invalid configuration file: " ++ configFile
        else do
            let [f'', f', sizeStr, totalItersStr, itersPerSecStr, modeStr] = words . head $ conf
            let mf          = (read f'', read f') :: (Int, Int)
            let s           = (read sizeStr) :: Int
            let mode        = read modeStr :: Mode
            let totalIters  = read totalItersStr :: Int
            let itersPerSec = read itersPerSecStr :: Int
            print $ "Config will be: " ++ show mf
            print $ "Size will be: " ++ sizeStr
            print $ "Mode will be: " ++ show mode
            print $ "Total iterations: " ++ show totalIters
            print $ "Iterations per second: " ++ show itersPerSec
            
            run mode totalIters itersPerSec s mf


