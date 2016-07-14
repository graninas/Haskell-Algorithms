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


data PropertyToken a where
     IntProperty :: Name -> Int -> PropertyToken Int
     IntResource :: Name -> (Int, Int) -> PropertyToken (Int, Int)

instance Show (PropertyToken Int) where
    show (IntProperty _ i) = show i

instance Show (PropertyToken (Int, Int)) where
    show (IntResource _ i) = show i

class (Show a) => Prop a where
    type Out a b :: *
    getProperty :: (a ~ Out b ()) => a -> Out a ()
    printProperty :: a -> String

instance Prop (PropertyToken a) where
    type Out (PropertyToken a) a = a
    getProperty (IntProperty _ k) = k
    getProperty (IntResource _ k) = k
    printProperty (IntProperty _ k) = show k
    printProperty (IntResource _ k) = show k

instance Show (PropertyToken a) where
    show (IntProperty _ k) = show k
    show (IntResource _ k) = show k


data Wrap = forall p. Prop p => MkWrap p

instance Show Wrap where
    show (MkWrap p) = show p

instance Prop Wrap where
    --type Out Wrap a = PropertyToken a
    getProperty (MkWrap p) = undefined
    printProperty = undefined

type PropList = [Wrap]

token1 = MkWrap $ IntProperty "int" 10
token2 = MkWrap $ IntResource "intResource" (10, 1000)
tokens = [token1, token2] :: PropList

main = do
    print $ getProperty (IntProperty "a" 10)
    print $ getProperty (IntResource "b" (20, 20))
    print $ map show tokens
    print tokens
    
    
    let res1 = getProperty $ head tokens
   -- putStrLn $ printProperty res1

    putStrLn "Ok."