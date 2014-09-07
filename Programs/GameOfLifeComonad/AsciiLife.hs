{-
    This code taken from Habrahabr:
    http://habrahabr.ru/post/225473/
-}

module AsciiLife where

import Control.Comonad
import Control.Applicative
import Universe
import Life

import System.Process (rawSystem)

renderLifeAscii :: Universe2 Cell -> String
renderLifeAscii = unlines . map concat . map (map renderCell) . takeRange2 (-7, -7) (20, 20)
    where renderCell Alive = "██"
          renderCell Dead  = "  "
          
main = do
    gameLoop $ fromList2 Dead cells

gameLoop :: Universe2 Cell -> IO a
gameLoop u = do
    getLine
    rawSystem "clear" []
    putStr $ renderLife u
    gameLoop (u =>> rule)
