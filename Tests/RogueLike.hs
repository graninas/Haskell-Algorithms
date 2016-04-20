module RogueLike where

data Effects a = Effects a (Effects a) | NoEffect
type Capability a = Effects a -> a
data Properties a = Property (Capability a) (Properties a) | NoProperties
data Object a = O (Effects a) (Properties a)

-- a :: EffectSystem




type Actor a = Object a -> Object a
type ActorF a = Actor a -> Actor a

instance Show Effect where
  show (E n i) = "(" ++ n ++ "=" ++ show i ++ ")"

instance Show (Effects a) where
  show NoCast = "No cast."
  show (C e c) = show e ++ " " ++ show c

instance Show (Caps a) where
  show NoCaps = "No caps."
  show (Cap c caps) = "(Caps: " {- ++ show c -} ++ ", " ++ show caps ++ ")"
  
instance Show a => Show (Object a) where
  show (O cast caps) = show $ applyCast cast caps

cast (E "" _) = error "No cast: empty effect."
cast e = C e noCast

effect "" _ = error "No effect: empty name."
effect n i = E n i

object = O

cap c = Cap c NoCaps

noCast :: Effects a
noCast = NoCast

noCaps :: Caps a
noCaps = NoCaps

noObj = object noCast (\_ -> noCaps)

addCapability :: (Effects a -> a) -> (Caps a -> a)
addCapability c = \caps -> case caps of
    NoCaps     -> c noCast
    Cap c2 css -> 
    
-- (Caps a) is monoid!!
mergeCaps :: Caps a -> Caps a -> Caps a
mergeCaps NoCaps NoCaps = noCaps
mergeCaps NoCaps caps   = caps
mergeCaps caps   NoCaps = caps
mergeCaps (Cap c1 caps1) second@(Cap c2 caps2) = Cap c1 (mergeCaps caps1 second)
-- TODO!
--    | caps2 `contain` c1 = error "Capability " ++ show c1 ++ " is already set." 
--    | caps1 `contain` c2 = error "Capability " ++ show c2 ++ " is already set." 
--    | otherwise = Cap c1 (mergeCaps c2 second)

-- (Effects a) is monoid!!
mergeCasts :: Effects a -> Effects a -> Effects a
mergeCasts NoCast NoCast = noCast
mergeCasts NoCast c      = c
mergeCasts c      NoCast = c
mergeCasts (C (E e1 i1) c1) xc@(C (E e2 i2) c2) = C (E e1 i1) (mergeCasts xc c1)

applyCast cast caps = "Effects to caps: TODO."

takeBuff :: Int -> (Effects Int, String) -> Int
takeBuff i (NoCast, _) = i
takeBuff i (C (E eName eVal) c, buffName) | eName == buffName = takeBuff (i + eVal) (c, buffName)
                                          | otherwise         = takeBuff i          (c, buffName)

(<~) = takeBuff

a # f = f a

mkActor :: Properties a -> Actor a
mkActor props = \(O c1 _) -> object c1 props

mkActor' :: Capability a -> Actor a
mkActor' c = \(O casts _) -> object casts (addCapability c)

actorF casts = \gen -> \(O c1 f1) -> gen (object (mergeCasts casts c1) f1)

extract gen = let (O c f) = (gen noObj) in O c f

--------------------------------------

hp e n = \c -> n <~ (c, e)

cold = "cold"
warm = "warm"

warmCast, coldCast :: ActorF a
coldCast = actorF $ (cast (effect cold 1))
warmCast = actorF $ (cast (effect warm 1))

freezeHP :: Int -> Capability Int
freezeHP hp = \c -> hp <~ (c, cold)

warmable :: Capability Int
warmable   c = 10 <~ (c, warm)
frozenable c = 80 <~ (c, cold)

box = mkActor' frozenable

boxCasted = let
    caster1 = box # warmCast
    caster2 = caster1 # coldCast
    caster3 = caster2 # coldCast
    in extract caster3
    
    

