{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

type Caption = String
type Name = String
type PlayerName = String

data IntProperty = IntProperty Name Int
    deriving (Show, Read, Eq)
data IntResource = IntResource Name (Int, Int)
    deriving (Show, Read, Eq)

class Prop a where
    type Out a :: *
    getProperty :: a -> Out a

class PropertyBag a where
    type Elem a :: *
    empty :: a
    insert :: Elem a -> a -> a
    toList :: a -> [Elem a]
    
instance Prop a => PropertyBag [a] where
    type Elem [a] = a
    empty = []
    toList l = l
    insert a l = a : l


instance Prop IntProperty where
    type Out IntProperty = Int
    getProperty (IntProperty _ k) = k

instance Prop IntResource where
    type Out IntResource = (Int, Int)
    getProperty (IntResource _ k) = k

{-
So, what?
f :: (PropertyBag a, Prop b, b ~ Elem a) => a -> [b]
f bag = toList bag

prop1 = IntProperty "aa" 10
b :: (PropertyBag a, Prop p, p ~ Elem a) => p -> a
b p = insert (getProperty p) empty
-}

main = putStrLn "Ok."

    