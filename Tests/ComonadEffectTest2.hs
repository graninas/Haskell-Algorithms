module RogueLike where

data Effect = E String Int
data Cast a = C Effect (Cast a)
data Object a = O (Cast a) (Cast a -> a)


noEffect :: Effect
noEffect = E "" 0

noCast :: Cast a
noCast = C noEffect noCast

toString :: Cast a -> String
toString = undefined

extract :: Object a -> a
extract (O c f) = f c


buff :: String -> Cast a -> Cast a
buff "" _ = noCast
buff "cold" (C (E "" i) c)        = (C (E "cold" 1)  c)
buff e (C (E e1 i) c) | e == e1   = (C (E e (i + 1)) c)
                      | otherwise = (C (E e1 i) (buff e c))

takeBuff :: Int -> (Cast Int, String) -> Int
takeBuff i (C (E "" eVal) _, _) = i
takeBuff i (C (E eName eVal) c, buffName) | eName == buffName = takeBuff (i + eVal) (c, buffName)
                                          | otherwise         = takeBuff i          (c, buffName)

(<~) = takeBuff
                      
                      
cold :: Object a -> a
cold (O c f) = f (buff "cold" c)

warm :: Object a -> a
warm (O c f) = f (buff "warm" c)

frozenable :: Cast Int -> Int
frozenable c = 100 <~ (c, "cold")

box :: Object Int
box = O noCast frozenable

addEffect :: (Object a -> a) -> Cast a -> Cast a
addEffect b (C e c) = undefined

--someBuff :: Object a -> a
--extend someBuff :: (Object a -> a) -> (Object a -> a)
--extend ::(Object a -> a)
--      -> ((Object a -> a) -> (Object a -> a))

extend :: (Object a -> a) -> (Object a -> a) -> (Object a -> a)
extend b = \b1 -> (\(O c f) -> f (addEffect b $ addEffect b1 c))













