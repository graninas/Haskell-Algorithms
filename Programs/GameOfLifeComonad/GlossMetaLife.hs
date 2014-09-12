module Main where

import Universe
import MetaLife
import SampleCells

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies

import Data.Monoid

data Mode = Play | Simulate
  deriving (Show, Read)

configFile = "config.txt"

windowCfg = InWindow "Cellular automata" (1024, 768) (500, 300)

side = 15.0
thickness = 0.5
aliveRadius = 5.0
deadRadius = 2.0

pickColor 0    = black
pickColor (-1) = red
pickColor 1    = blue
pickColor _    = green

pickCellRadius c | c == dead = deadRadius
                 | otherwise = aliveRadius

pickFactorRadius f = 7.0 + (fromIntegral f)

renderCell :: MetaCell -> Picture
renderCell ((df1', df2', df3'), (df1, df2, df3), c) = Pictures [
    Color black $ Circle (pickFactorRadius df2),
    Color (pickColor df3)  $ ThickCircle thickness (pickCellRadius c)
    ]

renderLife :: Universe2 MetaCell -> Picture
renderLife = toPicture . toList2

toPicture :: [[MetaCell]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [screenXShift..] picss)
  where
    screenXShift = (length picss) `div` (-2)
    screenYShift = screenXShift
    toPicture' (j, pics) = map (transCellX j) (zip [screenYShift..] pics)

transCellX :: Int -> (Int, MetaCell) -> Picture
transCellX j (i, mc) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (renderCell mc)

stepLife :: MetaFactor -> a -> Float -> Universe2 MetaCell -> Universe2 MetaCell
stepLife mf _ _ = stepLifeUniverse mf

type Universes = (Int, Int, [Universe2 MetaCell])

stepUniverses' :: Universes -> Universes
stepUniverses' (cur, end, us) | cur >= end = (cur, end, us)
                              | otherwise  = (cur + 1, end, us)
stepUniverses :: a -> Float -> Universes -> Universes
stepUniverses _ _ = stepUniverses'

initialModel :: Int -> Universe2 MetaCell
initialModel s = fromList2 s zeroCell metaCells

mkIteration ti s mf = (0, ti, universes)
  where universes = iterate (stepLifeUniverse mf) (initialModel s)

renderUniverses :: Universes -> Picture
renderUniverses (cur, _, us) = renderLife . head . drop cur $ us

eventHandler :: Event -> Universes -> Universes
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) us = stepUniverses' us
eventHandler _ us = us

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
            let [f1Str, f2Str, f3Str, f4Str, f5Str
                 , ra1Str, ra2Str, ra3Str
                 , sizeStr
                 , totalItersStr
                 , itersPerSecStr
                 , modeStr] = words . head $ conf
            let mf = (read f1Str, read f2Str, read f3Str, read f4Str, read f5Str
                     , read ra1Str, read ra2Str, read ra3Str) :: MetaFactor
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


