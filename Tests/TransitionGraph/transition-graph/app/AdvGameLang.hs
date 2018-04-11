{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module AdvGameLang where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

data AdventureLF a
  = PrintS String a
  | GetInput (String -> a)

type AdventureL = Free AdventureLF

instance Functor AdventureLF where
  fmap f (PrintS   s next)  = PrintS s (f next)
  fmap f (GetInput   nextF) = GetInput (f . nextF)

printS :: String -> AdventureL ()
printS s = liftF $ PrintS s ()

getInput :: AdventureL String
getInput = liftF $ GetInput id
