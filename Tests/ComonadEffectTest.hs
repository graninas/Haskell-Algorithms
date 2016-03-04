module RogueLike where

data Effect = E String Int
data Cast a = C Effect (Cast a)


type Capability a = Cast a -> a

data Object a = O (Cast a) (Capability a)

instance Show Effect where
  show (E "" i) = "noEffect"
  show (E n i) = "(" ++ n ++ ":" ++ show i ++ ")"
  
instance Show (Cast a) where
  show (C (E "" _) _) = ""
  show (C e c) = show e ++ " " ++ show c
  
instance Show a => Show (Object a) where
  show (O c f) = show $ f c

noEffect :: Effect
noEffect = E "" 0

noCast :: Cast a
noCast = C noEffect noCast

noObj = O noCast (\_ -> 0)
                      
takeBuff :: Int -> (Cast Int, String) -> Int
takeBuff i (C (E "" eVal) _, _) = i
takeBuff i (C (E eName eVal) c, buffName) | eName == buffName = takeBuff (i + eVal) (c, buffName)
                                          | otherwise         = takeBuff i          (c, buffName)

(<~) = takeBuff

hp e n = \c -> n <~ (c, e)

eff = E

coldC :: Cast a
coldC = C (eff cold 1) noCast

warmC :: Cast a
warmC = C (eff warm 1) noCast

inert :: Cast Int -> Int
inert _ = 44

mkActor :: Capability Int -> Object Int -> Object Int
mkActor caps = \(O c1 _) -> O c1 caps


-- (Cast a) is monoid!!
merge :: Cast a -> Cast a -> Cast a
merge (C (E "" _) _) (C (E "" _) _) = noCast
merge (C (E "" _) _) c = c
merge c (C (E "" _) _) = c
merge (C (E e1 i1) c1) xc@(C (E e2 i2) c2) = C (E e1 i1) (merge xc c1)

cast c (O c1 f) = f (merge c1 c)

castWarm :: Object a -> a
castWarm = cast warmC

castCold :: Object a -> a
castCold = cast coldC

a # f = f a

mergeF f1 f2 = f1 -- TODO!!!!

-- warmC :: Object a -> a

-- box' # warmC' :: (Object a -> Object a)
-- warmC' box' :: (Object a -> Object a)
-- warmC' :: (Object a -> Object a) -> (Object a -> Object a)

eff' e = \gen -> \(O c1 f1) -> gen (O (merge e c1) f1)

warmC', coldC' :: (Object a -> Object a) -> (Object a -> Object a)
coldC' = eff' coldC
warmC' = eff' warmC

-- extend :: (w a -> b) -> w a -> w b
--extend :: ((Object a -> Object a) -> Object a) -> ((Object a -> Object a) -> (Object a -> Object a))
extend = \genF -> \gen -> (\(O c1 f1) -> genF (\(O c2 f2) -> gen (O (merge c1 c2) (mergeF f1 f2)  )) )
--extract :: (Object a -> Object a) -> Object a
extract gen = let (O c f) = (gen noObj) in O c f





cold = "cold"
warm = "warm"

freezeHP :: Int -> Capability Int
freezeHP hp = \c -> hp <~ (c, cold)

warmable :: Capability Int
warmable   c = 10 <~ (c, warm)
frozenable c = 80 <~ (c, cold)

box = mkActor $ do
    frozenable
    warmable

boxCasted = let
    caster1 = box # warmC'
    caster2 = caster1 # coldC'
    caster3 = caster2 # coldC'
    in extract caster3
    
    

