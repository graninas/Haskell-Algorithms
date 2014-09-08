{-
    This code taken from Habrahabr:
    http://habrahabr.ru/post/225473/
-}
module Universe where

import Control.Comonad
import qualified Data.Vector as V
import Control.Exception.Base (assert)

type Current = Int
data Universe a = Universe (V.Vector a) Int
newtype Universe2 a = Universe2 { getUniverse2 :: Universe (Universe a) }

left :: Universe a -> Universe a
left (Universe v 0)   = Universe v (V.length v - 1)
left (Universe v cur) = Universe v (cur - 1)

right :: Universe a -> Universe a
right (Universe v cur) | cur >= (V.length v - 1) = Universe v 0
                       | otherwise               = Universe v (cur + 1)

--makeUniverse fl fr x = Universe (V.tail $ iterate fl x) x (V.tail $ iterate fr x)
makeUniverse fl fr x@(Universe v cur) = Universe (V.fromList vs) (s `div` 2)
  where
    s = V.length v
    lvs = if cur == 0     then [] else take (cur - 1)     . tail $ iterate fl x
    rvs = if cur == s - 1 then [] else take (s - cur - 1) . tail $ iterate fr x
    vs = lvs ++ [x] ++ rvs

instance Functor Universe where
    fmap f (Universe v cur) = Universe (V.map f v) cur

instance Comonad Universe where
    duplicate = makeUniverse left right
    extract (Universe v cur) = V.unsafeIndex v cur

takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) (Universe v _) | a <  0 = V.toList $ V.slice (1 - a) (b - a + 1) v
takeRange (a, b) (Universe v _) | a >= 0 = V.toList $ V.slice (a - 1) (b - a + 1) v

instance Functor Universe2 where
    fmap f = Universe2 . (fmap . fmap) f . getUniverse2

instance Comonad Universe2 where
    extract = extract . extract . getUniverse2
    duplicate = fmap Universe2 . Universe2 . shifted . shifted . getUniverse2
        where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
              shifted = makeUniverse (fmap left) (fmap right)
              
nearest3 :: Universe a -> [a]
nearest3 u = fmap extract [left u, u, right u]

nearest5 :: Universe a -> [a]
nearest5 u = fmap extract [left . left $ u, right . right $ u] ++ nearest3 u

nearest7 :: Universe a -> [a]
nearest7 u = fmap extract [left . left . left $ u, right . right . right $ u] ++ nearest5 u

fromListU :: Int -> a -> [a] -> Universe a
fromListU _ _ [] = error "Empty list."
fromListU 0 _ _ = error "Invalid size."
fromListU s zeroC xs | length xs > s = error "Invalid bounds."
                     | otherwise =
    let unfilled     = s - (length xs)
        unfR         = unfilled `div` 2
        unfL         = unfilled - unfR
        rsL          = replicate unfL zeroC
        rsR          = replicate unfR zeroC
        xs'          = rsL ++ xs ++ rsR
    in assert (length xs' == s) $
        Universe (V.fromList xs') (s `div` 2)

fromList2 :: Int -> a -> [[a]] -> Universe2 a
fromList2 s zeroC xss = Universe2 vss
  where
    zeroU = fromListU s zeroC . replicate s $ zeroC
    vs    = map (fromListU s zeroC) xss
    vss   = fromListU s zeroU vs


