module TinkAndKiki where

type Arrow = Char
type Field = [[Arrow]]

type Position = (Int, Int)
type Positions = [Position]
type Dislocation = (Position, Position)

type Path = [Dislocation]
type Paths = [Path]

u, d, l, r :: Char
u = 'u'
d = 'd'
l = 'l'
r = 'r'

field :: Field
field = [ [u, l, u, r, l]
        , [u, d, d, r, r]
        , [l, r, r, l, l]
        , [d, r, u, u, u]
        , [d, d, r, l, u] ]

energyLimit = 10

tinkStart, kikiStart :: Position
tinkStart = (0, 0)
kikiStart = (4, 4)

startPositions :: Dislocation
startPositions = (tinkStart, kikiStart)


