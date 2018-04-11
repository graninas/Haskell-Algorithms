{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures                #-}

module TransitionGraph.Graph where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

type Event = String

data TransitionF lang b o u
  = Backable    Event (Graph lang b o) u
  | ForwardOnly Event (Graph lang b o) u

type Transition lang b o u = Free (TransitionF lang b o) u

data GraphF lang i o b
  = GraphF  (     lang b) (Transition lang b o ())
  | GraphF1 (i -> lang b) (Transition lang b o ())

newtype Graph lang i o
  = Graph (Exists (GraphF lang i o))

type PartialTrans lang i o b = Transition lang b o () -> Graph lang i o
data Event' lang i o = Event' Event (Graph lang i o)

instance Functor (TransitionF lang b o) where
  fmap f (Backable    e g next) = Backable    e g (f next)
  fmap f (ForwardOnly e g next) = ForwardOnly e g (f next)

(<~>) = transable backable
(~>)  = transable forwardOnly

infixl 3 <~>
infixl 3 ~>

with
  :: (Monad lang)
  => lang b
  -> Transition lang b o ()
  -> Graph lang () o
with flow table = Graph $ mkExists $ GraphF flow table

with1
  :: (Monad lang)
  => (i -> lang b)
  -> Transition lang b o ()
  -> Graph lang i o
with1 flowF1 table = Graph $ mkExists $ GraphF1 flowF1 table

leaf
  :: (Monad lang)
  => lang ()
  -> Graph lang () ()
leaf flow = with flow (pure ())

leaf1
  :: (Monad lang)
  => (i -> lang ())
  -> Graph lang i ()
leaf1 flowF1 = with1 flowF1 (pure ())

graph part = part $ pure ()

on
  :: Event
  -> Graph lang i o
  -- -> (forall (lang :: * -> *). Graph lang i o)
  -> Event' lang i o
on = Event'

transable
  :: (Event
      -> Graph lang i o
      -> Free (TransitionF lang i o) b
      )
  -> (Free (TransitionF lang i o) b -> c)
  -> Event' lang i o
  -> Free (TransitionF lang i o) a
  -> c

transable transType part (Event' e g) = part . transed
  where
    transed prevTrans = do
      prevTrans
      transType e g

backable
  :: Event
  -> Graph lang i o
  -> Transition lang i o ()
backable e g = liftF $ Backable e g ()

forwardOnly
  :: Event
  -> Graph lang i o
  -> Transition lang i o ()
forwardOnly e g = liftF $ ForwardOnly e g ()
