module Main where

-- | Подключение модулей работы со списками (Data.List) и с монадами (Control.Monad).
--   Модули подключаются под именами L и M соответственно.
--   Из модуля Control.Monad берется только функция guard.
import qualified Data.List as L
import qualified Control.Monad as M (guard)

-- | Используемые в программе типы данных.
type DeviceData      = (Char, Float)   -- | Данные по устройству: символ и вероятность выхода из строя.
type DevicesData     = [DeviceData]    -- | Список данных по устройствам.
type FailureCostData = [Float]         -- | Суммы убытков каждого из отказов.
type ProblemData     = (DevicesData, FailureCostData) -- | Данные задачи.
type Damages         = [Float]                        -- | Убыточность устройств.

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


-- | Возвращает список из символьных имен устройств.
deviceNames :: DevicesData -> [Char]
deviceNames = map fst

-- | Возвращает сумму вероятностей устройств.
sums [] = 0
sums ((_,pa):aas) = pa + sums aas

-- | Высчитывает вероятность для варианта перестановки.
--   Например, вероятность варианта "BAC", если есть всего 3 устройства.
variantProbability xs = f xs (sums xs)
  where
    f [] _ = ("", 1)
    f ((dev, p):xs) sum = let
        (devs, probs) = f xs (sum - p)
        prob          = (p / sum)
        in (dev : devs, prob * probs)

-- | Высчитывает вероятности всех вариантов перестановки.
--   Это самая затратная функция, так как она содержит функцию перестановок -
--   permutations, которая работает со сложностью O(n!).
variantProbabilities = map variantProbability . L.permutations

-- | Вычисляет значение ячейки на позиции (dev, pos) в результирующей таблице.
cell xs c@(dev, pos) = (c, sum devProbs)
  where
    devProbs = do
        varProb <- xs
        M.guard (pos `elem` (L.elemIndices dev . fst $ varProb))
        return (snd varProb)

-- | Вычисляет вероятность выхода из строя устройства на всех позициях (фактически - строка результирующей таблицы).
--   Функция работает неэффективно, так как вызывает функцию variantProbabilities, возможно, много раз.
devicePositionProb devices colsCount devName = do
    col <- [0..colsCount - 1]
    return (cell (variantProbabilities devices) (devName, col))

-- | Вычисляет ущерб из-за выхода из строя определенного устройства.
--   Функция работает очень неэффективно.
damage (devices, failCosts) devName = let
    colsCount            = length failCosts
    devPosProbs          = devicePositionProb devices colsCount devName
    producted            = zipWith (\x y -> snd x * y) devPosProbs failCosts
    in sum producted

-- | Вычисляет результирующую таблицу вероятностей.
--   Функция работает очень неэффективно.
probabilityTable (devices, failCosts)  = do
    devName <- deviceNames devices
    let colsCount = (length failCosts) - 1
    return (devicePositionProb devices colsCount devName)

-- | Вычисляет ущерб выхода из строя для каждого устройства.
damages :: ProblemData -> Damages
damages problemData = do
    devName <- deviceNames (fst problemData)
    return (damage problemData devName)

-- | Точка входа в программу.
main = do
    let results = damages problem3
    putStrLn . show $ results




-- Другие варианты тех же функций.

sums' = foldr (\(_, p) -> (p +)) 0

variantProbability' [] = ("", 1)
variantProbability' d@((dev, p) : xs) = let
    (devs, probs) = variantProbability' xs
    prob          = (p / sums d)
    in (dev : devs, prob * probs)

variantProbability'' [] = ("", 1)
variantProbability'' ((dev, p):xs) = let
    (devs, probs) = variantProbability'' xs
    prob          = p / (p + sums xs)
    in (dev : devs, prob * probs)

cell' xs c@(dev, pos) = let
    devProbs = [snd varProb | varProb <- xs, pos `elem` (L.elemIndices dev . fst $ varProb)]
    in (c, sum devProbs)
