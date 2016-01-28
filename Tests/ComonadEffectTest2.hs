module RogueLike where

data Effect = E String Int
data Cast a = C Effect (Cast a)
data Object a = O (Cast a) (Cast a -> a)


instance Show Effect where
  show (E "" i) = "noEffect"
  show (E n i) = "(" ++ n ++ ":" ++ show i ++ ")"
  
instance Show (Cast a) where
  show (C (E "" _) _) = ""
  show (C e c) = show e ++ " " ++ show c

noEffect :: Effect
noEffect = E "" 0

noCast :: Cast a
noCast = C noEffect noCast

toString :: Cast a -> String
toString = undefined

{-
buff :: String -> Cast a -> Cast a
buff "" _ = noCast
--buff "cold" (C (E "" i) c)        = (C (E "cold" 1)  c)
buff e (C (E e1 i) c) | e == e1   = (C (E e (i + 1)) c)
                      | otherwise = (C (E e1 i) (buff e c))
                      
coldBuff = buff "cold" noCast
warmBuff = buff "warm" noCast
-}
                      
                      
takeBuff :: Int -> (Cast Int, String) -> Int
takeBuff i (C (E "" eVal) _, _) = i
takeBuff i (C (E eName eVal) c, buffName) | eName == buffName = takeBuff (i + eVal) (c, buffName)
                                          | otherwise         = takeBuff i          (c, buffName)

(<~) = takeBuff

cold = "cold"
warm = "warm"

eff = E

coldC :: Cast a
coldC = C (eff cold 1) noCast

warmC :: Cast a
warmC = C (eff warm 1) noCast
                      
frozenable :: Cast Int -> Int
frozenable c = 100 <~ (c, "cold")

warmable :: Cast Int -> Int
warmable c = 10 <~ (c, "warm")

box :: Object Int
box = O noCast frozenable

-- (Cast a) is monoid!!
merge :: Cast a -> Cast a -> Cast a
merge (C (E "" _) _) (C (E "" _) _) = noCast
merge (C (E "" _) _) c = c
merge c (C (E "" _) _) = c
merge (C (E e1 i1) c1) xc@(C (E e2 i2) c2) = C (E e1 i1) (merge xc c1)
--    | e1 == e2  = merge (C (E e1 (i1 + i2)) c1) c2
--    | otherwise = C (E e1 i1) (merge xc c1)

extract :: Object a -> a
extract (O c f) = f c

cast c (O c1 f) = f (merge c1 c)

castWarm :: Object a -> a
castWarm = cast warmC

castCold :: Object a -> a
castCold = cast coldC

mergeF f1 f2 = f1 -- TODO!!!!

-- extract box :: a
-- extract :: Object a -> a

-- objCr :: aaa -> a
-- caster1 :: aaa -> a
-- caster2 :: aaa -> a
-- let caster1 = objCr # warmC'
-- let caster2 = caster1 # coldC'
-- let i = extract caster2
-- extract caster2 :: a
-- extract :: (aaa -> a) -> a
-- warmC' :: (aaa -> a) -> (aaa -> a)
-- coldC' :: (aaa -> a) -> (aaa -> a)



{-

--cold :: Object a -> a
--cold (O c f) = f (coldBuff c)

--warm :: Object a -> a
--warm (O c f) = f (warmBuff c)

--warm' b (O c f) = \(O c' _) -> b (O (merge c' $ buff "warm" c) f)
--cold' b = \(O c f) -> b (O (buff "cold" c) f)

addEffect :: Cast a -> (Object a -> a) -> Cast a
addEffect (C e c) = \(O c f) -> undefined

(<<<) = addEffect
infixl 5 <<<

--someBuff :: Object a -> a
--extend someBuff :: (Object a -> a) -> (Object a -> a)
--extend ::(Object a -> a)
--      -> ((Object a -> a) -> (Object a -> a))

extend :: (Object a -> a) -> (Object a -> a) -> (Object a -> a)
extend b = \b1 -> (\(O c f) -> f (c <<< b1 <<< b))


-}










