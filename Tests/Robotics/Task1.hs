module Main where

u = 'u'
l = 'l'
r = 'r'
d = 'd'

minIndex = 0
maxIndex = 4

field :: [[Char]]
field = [ [l, u, u, d, r]
        , [l, l, r, l, l]
        , [d, d, d, d, l]
        , [u, l, d, r, l]
        , [u, l, d, d, u] ]

tinkStart = (0, 0)
kikiStart = (4, 4)
        
moves (x, y) = [(x1, y1) | x1 <- [x - 1, x, x + 1], x1 >= minIndex, x1 <= maxIndex
                         , y1 <- [y - 1, y, y + 1], y1 >= minIndex, y1 <= maxIndex
                         , (x1, y1) /= (x, y)]

moveKiki (kx, ky) action = (moveX kx action, moveY ky action)
  where
    moveX x a | a == u && x == minIndex = 0
    moveX x a | a == d && x == maxIndex = x
    

                         
step (tink, kiki) (tx, ty) = let
    kikiAction = (field !! tx) !! ty
    kikiPos = moveKiki kiki kikiAction