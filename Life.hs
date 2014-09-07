module Main where

import Control.Comonad
import Control.Applicative
import System.Process (rawSystem)

import Universe

data Cell = Dead | Alive
    deriving (Eq, Show)

nearest3 :: Universe a -> [a]
nearest3 u = fmap extract [left u, u, right u]

neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)

rule :: Universe2 Cell -> Cell
rule u
    | nc == 2   = extract u
    | nc == 3   = Alive
    | otherwise = Dead
    where nc = length $ filter (==Alive) (neighbours u)

renderLife :: Universe2 Cell -> String
renderLife = unlines . map concat . map (map renderCell) . takeRange2 (-7, -7) (20, 20)
    where renderCell Alive = "██"
          renderCell Dead  = "  "

fromList :: a -> [a] -> Universe a
fromList d (x:xs) = Universe (repeat d) x (xs ++ repeat d)

fromList2 :: a -> [[a]] -> Universe2 a
fromList2 d = Universe2 . fromList ud . fmap (fromList d)
    where ud = Universe (repeat d) d (repeat d)

cells = [ [ Dead, Alive,  Dead]
        , [Alive,  Dead,  Dead]
        , [Alive, Alive, Alive] ]

main = do
    gameLoop $ fromList2 Dead cells

gameLoop :: Universe2 Cell -> IO a
gameLoop u = do
--    getLine
    rawSystem "clear" []
    putStr $ renderLife u
    gameLoop (u =>> rule)
