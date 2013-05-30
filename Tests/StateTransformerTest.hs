{-# LANGUAGE PackageImports #-}

module Main where

import "mtl" Control.Monad.State.Lazy
import "mtl" Control.Monad (when)

rollDice :: (Int, Int) -> IO Int
rollDice (l, r) = randomRIO (l, r)



data Player = Player { name :: String
                                 , awards :: [Maybe String] }

data GameData = GameData

                         { totalScore :: Int,
                           turn :: Int,
                           players :: [Player] }

type GS a = (StateT GameData IO a)


main = do
    putStrLn "Let dice you!"
    d20 <- rollDice (1, 20)
    putStrLn $ "Value: " ++ show d20
    case d20 >= 17 of
        True  -> putStrLn "LUCKY!!"
        False -> putStrLn "No..."


type Locations = M.Map Room Location
type MaybeLocation = MaybeSomething Location

data GameState = GameState {
        gsLocations :: Locations,
        gsCurrentRoom :: Room,
        gsObjects :: Objects
    } deriving (Show, Read)

type GS a = (StateT GameState IO a)


-- the type of an 'action' (weld, dunk, etc.)
type GameAction = Object -> Object -> GameState Result


data GS = GS { worldMap :: [Location]
             , currentLocation :: Location
             , welded :: Bool
             , bucketFull :: Bool } 
    deriving (Show)

newtype GameState a = GameState
            { runGameState :: StateT GS IO a }
    deriving (Monad, MonadIO, MonadState GS)