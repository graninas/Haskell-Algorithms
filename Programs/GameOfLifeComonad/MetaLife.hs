{-# LANGUAGE TupleSections #-}

module MetaLife where

import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Vector as V

import Universe

type Cell = Int
dead  = 0
alive = 1

data RuleArea = Ring1 | Ring2 | Ring3
  deriving (Show, Read)

-- dfOtL for df Old Threshold Left  Limit
-- dfMt for df Middle Threshold Limit
-- dftR for df Threshold Right
-- Classic GOL:


type DfOtL = Int
type DfMt = Int
type DftR = Int
type RuleModifiers   = (DfOtL, DfMt, DftR)
type DfModifiers     = (DfOtL, DfMt, DftR)
type MetaCell        = (DfModifiers, RuleModifiers, Cell)
type MetaFactor      = (Int, Int, Int, Int, Int, RuleArea, RuleArea, RuleArea)

zeroModifiers       = (0, 0, 0)
zeroCellCreator c   = (zeroModifiers, zeroModifiers, c)
zeroCell :: MetaCell
zeroCell = zeroCellCreator dead

stepLifeUniverse mf = (=>> (rule'' mf))
isAlive :: MetaCell -> Bool
isAlive (_, _, c) = c == alive

rule'' :: MetaFactor -> Universe2 MetaCell -> MetaCell
rule'' mf@(_, _, _, f4, f5, _, _, r3) u
--    | nc < f4      = ((dfOtL', dfMt', dftR'), oldMods, new)
--    | nc < f4 + f5 = ((1, 1, 1),              oldMods, new)
    | otherwise    = ((1, 0, 1),           oldMods, new)
  where
    old@( (dfOtL', dfMt', dftR')
        , oldMods
        , c) = extract u
    (_, _, new) = rule' mf u
    ruleArea = pickRuleArea r3
    nc       = length $ filter isAlive (parNeighbours ruleArea u)

rule' :: MetaFactor -> Universe2 MetaCell -> MetaCell
rule' mf@(f1, f2, f3, _, _, _, r2, _) u
    | nc < f1           = (oldDfMods, (dfOtL' + dfOtL, dfMt', dftR' + dftR),     new)
    | nc < f1 + f2      = (oldDfMods, (dfOtL, dfMt,  dftR),                 new)
    | otherwise         = (oldDfMods, (dfOtL' - dfOtL, dfMt', dftR' - dftR), new)
  where
    old@( oldDfMods@(dfOtL', dfMt', dftR')
        , oldMods  @(dfOtL,  dfMt,  dftR)
        , c) = extract u
    (_, _, new) = rule mf u
    ruleArea = pickRuleArea r2
    nc = length $ filter isAlive (parNeighbours ruleArea u)

rule :: MetaFactor -> Universe2 MetaCell -> MetaCell
rule mf@(_, _, _, _, _, r1, _, _) u
    | nc >= (2 - dfOtL) && nc < (3 + dfMt) = old
    | nc >= (3 + dfMt)  && nc < (4 + dftR) = (oldDf', oldDf, alive)
    | otherwise                            = (oldDf', oldDf, dead)
    where
        ruleArea = pickRuleArea r1
        old@(oldDf', oldDf@(dfOtL, dfMt, dftR), c) = extract u
        nc = length $ filter isAlive (parNeighbours ruleArea u)

pickRuleArea Ring1 = neighbours
pickRuleArea Ring2 = neighbours'
pickRuleArea Ring3 = neighbours''

parNeighbours :: NFData a => ((Universe2 a) -> [a]) -> (Universe2 a) -> [a]
parNeighbours ns u = runEval $ parList rpar (force $ ns u)
   
neighbours'' :: (Universe2 a) -> [a]
neighbours'' u =
    [ nearest7 . extract . left3
    , pure     . extract . left3  . extract . left
    , pure     . extract . right3 . extract . left
    , pure     . extract . left3  . extract . left2
    , pure     . extract . right3 . extract . left2
    , pure     . extract . left3  . extract
    , pure     . extract . right3 . extract
    , pure     . extract . left3  . extract . right
    , pure     . extract . right3 . extract . right
    , pure     . extract . left3  . extract . right2
    , pure     . extract . right3 . extract . right2
    , nearest7 . extract . right3
    ] >>= ($ getUniverse2 u)

neighbours' :: (Universe2 a) -> [a]
neighbours' u =
    [ nearest5 . extract . left2
    , pure     . extract . left2  . extract . left
    , pure     . extract . right2 . extract . left
    , pure     . extract . left2  . extract
    , pure     . extract . right2 . extract
    , pure     . extract . left2  . extract . right
    , pure     . extract . right2 . extract . right
    , nearest5 . extract . right2
    ] >>= ($ getUniverse2 u)

neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)



