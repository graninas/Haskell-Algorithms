{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module AdvGame.Lang where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

type Item = String

data AdventureLF a
  = PrintS String a
  | Put Item a
  | Drop Item a
  | List a

type AdventureL = Free AdventureLF

instance Functor AdventureLF where
  fmap f (PrintS s next)  = PrintS s (f next)
  fmap f (Put    s next)  = Put    s (f next)
  fmap f (Drop   s next)  = Drop   s (f next)
  fmap f (List     next)  = List     (f next)

printS :: String -> AdventureL ()
printS s = liftF $ PrintS s ()

put :: String -> AdventureL ()
put s = liftF $ Put s ()

drop :: String -> AdventureL ()
drop s = liftF $ Drop s ()

list :: AdventureL ()
list = liftF $ List ()

nop :: AdventureL ()
nop = pure ()
