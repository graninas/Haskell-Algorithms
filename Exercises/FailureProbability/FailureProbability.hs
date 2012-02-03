{-# LANGUAGE BangPatterns #-}

module Main where

-- | Подключение модулей работы со списками (Data.List) и с монадами (Control.Monad).
--   Модули подключаются под именами L и M соответственно.
--   Из модуля Control.Monad берется только функция guard.
import qualified Data.List as L
import qualified Control.Monad as M (guard)
import qualified Data.Map as Map

-- | Используемые в программе типы данных.
type DeviceData         = (Char, Float)   -- | Данные по устройству: символ и вероятность выхода из строя.
type DevicesData        = [DeviceData]    -- | Список данных по устройствам.
type FailureCostData    = [Float]         -- | Суммы убытков каждого из отказов.
type ProblemData        = (DevicesData, FailureCostData) -- | Данные задачи.
type VariantProbability = (String, Float)

type ProbMapKey     = (Char, Int)
type ProbabilityMap = Map.Map ProbMapKey Float
type DeviceCostMap  = Map.Map Char       Float


emptyProbMap :: ProbabilityMap
emptyProbMap = Map.empty
emptyDeviceCostMap :: DeviceCostMap
emptyDeviceCostMap = Map.empty


-- | Данные по устройствам.
devices1, devices2 :: DevicesData
devices1 = [ ('A', 0.37)
           , ('B', 0.43)
           , ('C', 0.2) ]

devices2 = [ ('A', 0.17)
           , ('B', 0.13)
           , ('C', 0.20)
           , ('D', 0.09)
           , ('E', 0.40)
           , ('F', 0.01) ]

devices3 = [ ('A', 0.10)
           , ('B', 0.11)
           , ('C', 0.05)
           , ('D', 0.04)
           , ('E', 0.14)
           , ('F', 0.10)
           , ('G', 0.10)
           , ('H', 0.09)
           , ('I', 0.15)
           , ('J', 0.12) ]

-- | Суммы убытков из-за отказов.
failureCost1, failureCost2 :: FailureCostData
failureCost1 = [20, 20, 30, 30]
failureCost2 = [ 8, 12, 20, 60]
failureCost3 = [ 1,  3, 11, 34, 51]

-- | Полные данные по примерам 1, 2, 3.
problem1, problem2, problem3 :: ProblemData
problem1 = (devices1, failureCost1)
problem2 = (devices2, failureCost2)
problem3 = (devices3, failureCost3)

-- | Возвращает сумму вероятностей устройств.
probabilitySum [] = 0
probabilitySum ((_,pa):aas) = pa + probabilitySum aas

-- | Высчитывает вероятность для варианта перестановки.
--   Например, вероятность варианта "BACD", если есть всего 4 устройства.
variantProbability _ []                 = ([],    1)
variantProbability _ ((dev,_):[])       = ([dev], 1)
variantProbability varSum ((dev, p):xs) = let
        (devs, probs) = variantProbability (varSum - p) xs
        prob          = (p / varSum)
        in (dev : devs, prob * probs)

variantProbabilities :: DevicesData -> [VariantProbability]
variantProbabilities devices = let
    varSum = probabilitySum devices
    perms  = L.permutations devices
    in map (variantProbability varSum) perms

probabilityTable :: [VariantProbability] -> ProbabilityMap
probabilityTable varProbs = L.foldl' f emptyProbMap varProbs
  where
    f :: ProbabilityMap -> VariantProbability -> ProbabilityMap
    f dataMap (varString, prob) = snd $ L.foldl' (f' prob) (0, dataMap) varString
    f' :: Float -> (Int, ProbabilityMap) -> Char -> (Int, ProbabilityMap)
    f' prob (idx, dataMap) dev = let
        key = (dev, idx)
        in case Map.lookup key dataMap of
            Just x  -> (idx + 1, Map.insert key (prob + x) dataMap)
            Nothing -> (idx + 1, Map.insert key  prob      dataMap)


deviceCosts :: FailureCostData -> ProbabilityMap -> DeviceCostMap
deviceCosts failureCost dataMap = result
  where
    result = Map.foldrWithKey (f infFailCost) emptyDeviceCostMap dataMap
    infFailCost = failureCost ++ repeat 0 -- Бесконечный список для охвата всех индексов idx
    f :: FailureCostData -> ProbMapKey -> Float -> DeviceCostMap -> DeviceCostMap
    f fCost (ch, idx) prob m = let
        producted = prob * (fCost !! idx)
        in case Map.lookup ch m of
            Just p  -> Map.insert ch (p + producted) m
            Nothing -> Map.insert ch producted m

-- | Точка входа в программу.
main = do
    let (devices, failureCost) = problem2
    let !varProbs  = variantProbabilities devices
    let probTable  = probabilityTable varProbs
    let devCosts   = deviceCosts failureCost probTable

    putStrLn . show $ devCosts
    putStrLn "Ok."
