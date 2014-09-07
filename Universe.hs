{-
    This code taken from Habrahabr:
    http://habrahabr.ru/post/225473/
-}
module Universe where

import Control.Comonad

data Universe a = Universe [a] a [a]
newtype Universe2 a = Universe2 { getUniverse2 :: Universe (Universe a) }

left :: Universe a -> Universe a
left  (Universe (a:as) x bs) = Universe as a (x:bs)

right :: Universe a -> Universe a
right (Universe as x (b:bs)) = Universe (x:as) b bs

makeUniverse fl fr x = Universe (tail $ iterate fl x) x (tail $ iterate fr x)

instance Functor Universe where
    fmap f (Universe as x bs) = Universe (fmap f as) (f x) (fmap f bs)

instance Comonad Universe where
    duplicate = makeUniverse left right
    extract (Universe _ x _) = x

takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) u = take (b-a+1) x
    where Universe _ _ x
            | a < 0 = iterate left u !! (-a+1)
            | otherwise = iterate right u !! (a-1)

instance Functor Universe2 where
    fmap f = Universe2 . (fmap . fmap) f . getUniverse2

instance Comonad Universe2 where
    extract = extract . extract . getUniverse2
    duplicate = fmap Universe2 . Universe2 . shifted . shifted . getUniverse2
        where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
              shifted = makeUniverse (fmap left) (fmap right)

takeRange2 :: (Int, Int) -> (Int, Int) -> Universe2 a -> [[a]]
takeRange2 (x0, y0) (x1, y1)
    = takeRange (y0, y1)
    . fmap (takeRange (x0, x1))
    . getUniverse2
