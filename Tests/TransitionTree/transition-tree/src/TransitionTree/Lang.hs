{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionTree.Lang where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

data LangF a
  = PrintS String a
  | GetInput (String -> a)

type Lang a = Free LangF a

instance Functor LangF where
  fmap f (PrintS   s next)  = PrintS s (f next)
  fmap f (GetInput   nextF) = GetInput (f . nextF)

printS :: String -> Lang ()
printS s = liftF $ PrintS s ()

getInput :: Lang String
getInput = liftF $ GetInput id