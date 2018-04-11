{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Graph where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

import           TransitionGraph.Lang

type Event = String

data TransitionF b o u
  = Backable    Event (Graph b o) u
  | ForwardOnly Event (Graph b o) u

type Transition b o u = Free (TransitionF b o) u

data GraphF i o b
  = GraphF       (Lang b) (Transition b o ())
  | GraphF1 (i -> Lang b) (Transition b o ())

newtype Graph i o = Graph (Exists (GraphF i o))

type PartialTrans i o b = Transition b o () -> Graph i o
data Event' i o = Event' Event (Graph i o)

instance Functor (TransitionF b o) where
  fmap f (Backable    e g next) = Backable    e g (f next)
  fmap f (ForwardOnly e g next) = ForwardOnly e g (f next)

(<~>) = backable''
(~>) = forwardOnly''

infixl 3 <~>
infixl 3 ~>

with :: forall b o. Lang b -> Transition b o () -> Graph () o
with flow table = Graph $ mkExists $ GraphF flow table

with1 :: forall i b o. (i -> Lang b) -> Transition b o () -> Graph i o
with1 flowF1 table = Graph $ mkExists $ GraphF1 flowF1 table

leaf :: Lang () -> Graph () ()
leaf flow = with flow (pure ())

leaf1 :: forall i. (i -> Lang ()) -> Graph i ()
leaf1 flowF1 = with1 flowF1 (pure ())

graph part = part $ pure ()

on = Event'

transable transType part (Event' e g) = part . transed
  where
    transed prevTrans = do
      prevTrans
      transType e g

backable' :: Event -> Graph i o -> Transition i o ()
backable' e g = liftF $ Backable e g ()

forwardOnly' :: Event -> Graph i o -> Transition i o ()
forwardOnly' e g = liftF $ ForwardOnly e g ()

backable'' = transable backable'
forwardOnly'' = transable forwardOnly'
