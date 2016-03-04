data Effect = E String Int
data Cast a = C Effect (Cast a)
type Capability a = Cast a -> a
data Object a = O (Cast a) (Capability a)

mkActor :: Capability Int -> Object Int -> Object Int
mkActor caps = \(O c1 _) -> O c1 caps

takeBuff :: Int -> (Cast Int, String) -> Int
takeBuff i (C (E "" eVal) _, _) = i
takeBuff i (C (E eName eVal) c, buffName) | eName == buffName = takeBuff (i + eVal) (c, buffName)
                                          | otherwise         = takeBuff i          (c, buffName)

(<~) = takeBuff

------------------------------

warmable :: Capability Int
warmable   c = 10 <~ (c, "warm")
frozenable c = 80 <~ (c, "cold")

-- WTF?!! How does it work?!!!
box = mkActor $ do
    warmable
    frozenable
