module Main where

import System.Random
import Control.Monad.State (get, gets, StateT(..), evalStateT,
                            put, MonadState(..), liftIO)

data Player = Player { name :: String
                     , awards :: [Maybe String] }

data GameData = GameData
                         { totalScore :: Int,
                           try :: Int,
                           players :: [Player] }

type GS a = (StateT GameData IO a)

getAward 20 = Just "Awesome!!!"
getAward 19 = Just "Great!"
getAward n | n > 16 = Just "Very well."
getAward _  = Nothing

turn :: Int -> GS (Maybe String)
turn player = do
    dice <- liftIO $ randomRIO (1, 20) :: IO Int
    let award = getAward dice
    putStrLn $ "Award: " ++ show award
    
    (GameData score try players) <- get
    put (GameData score (try + 1) players)
    return award