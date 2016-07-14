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

class Show a => Prop a where
    type Out a :: *
    getProperty :: a -> Out a

instance Prop IntProperty where
    type Out IntProperty = Int
    getProperty (IntProperty _ k) = k

instance Prop IntResource where
    type Out IntResource = (Int, Int)
    getProperty (IntResource _ k) = k


data PropertyToken = forall p. Prop p => MkPropertyToken p
type PropertyTokens = [PropertyToken]

data RawToken = Item Name PropertyTokens

instance Show PropertyToken where
    show (MkPropertyToken p) = show p



token1 = MkPropertyToken (IntProperty "int" 10)
token2 = MkPropertyToken (IntResource "intResource" (10, 1000))
tokens = [token1, token2]

rawToken = Item "RawToken" tokens

main = do
    print $ getProperty (IntProperty "a" 10)
    print $ getProperty (IntResource "b" (20, 20))

    putStrLn "Ok."