module RogueLike where

type Apply a = (a, Int) -> (a, Int)
data Effect a = E ((a, Int), Apply a) (Effect a)
-- data Effect a = E (a, a -> a) (Effect a)

noApply :: Apply a
noApply = id

undefE = ("", 0)

noEffect :: Effect String
noEffect = E (undefE, noApply) noEffect


 
toString :: Effect a -> String
toString = undefined


inc :: Apply a
inc (a, b) = (a, b + 1)

dec :: Apply a
dec (a, b) = (a, b - 1)


extract (E (a, _) _) = a

getE :: a -> Effect a -> a
getE a (E (e, _) _) = (a, 0)





