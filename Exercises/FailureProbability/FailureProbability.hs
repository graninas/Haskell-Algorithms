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
type Damages            = [Float]                        -- | Убыточность устройств.
type VariantProbability = (String, Float)

type MapKey = (Char, Int)
type ProbabilityMap = Map.Map MapKey Float


emptyProbMap :: ProbabilityMap
emptyProbMap = Map.empty


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
probabilityTable varProbs = foldr f emptyProbMap varProbs
  where
    f :: VariantProbability -> ProbabilityMap -> ProbabilityMap
    f (varString, prob) dataMap = snd $ foldl (f' prob) (0, dataMap) varString
    f' :: Float -> (Int, ProbabilityMap) -> Char -> (Int, ProbabilityMap)
    f' prob (idx, dataMap) dev = let
        key = (dev, idx)
        in case Map.lookup key dataMap of
            Just x  -> (idx + 1, Map.insert key (prob + x) dataMap)
            Nothing -> (idx + 1, Map.insert key prob dataMap)

{-
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr, applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left:

 foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

insert :: Ord k => k -> a -> Map k a -> Map k a	Source

O(log n). Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value. insert is equivalent to insertWith const.

 insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
 insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
 insert 5 'x' empty                         == singleton 5 'x'

 
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a	Source

O(log n). The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.

 let f x = if x == "a" then Just "new a" else Nothing
 update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
 update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
 update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"





lookup :: Ord k => k -> Map k a -> Maybe a	Source

O(log n). Lookup the value at a key in the map.

The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.

An example of using lookup:

 import Prelude hiding (lookup)
 import Data.Map

 employeeDept = fromList([("John","Sales"), ("Bob","IT")])
 deptCountry = fromList([("IT","USA"), ("Sales","France")])
 countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])

 employeeCurrency :: String -> Maybe String
 employeeCurrency name = do
     dept <- lookup name employeeDept
     country <- lookup dept deptCountry
     lookup country countryCurrency

 main = do
     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))

The output of this program:

   John's currency: Just "Euro"
   Pete's currency: Nothing

   
   
   


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
-}

-- | Точка входа в программу.
main = do
    let (devices, failureCost) = problem1
    let varProbs = variantProbabilities devices
    let probTable = probabilityTable varProbs
    
    putStrLn . show $ probTable

    putStrLn "Ok."

{-
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
-}