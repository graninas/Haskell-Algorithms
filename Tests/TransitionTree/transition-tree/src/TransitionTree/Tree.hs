{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionTree.Tree where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

import           TransitionTree.Lang

type Event = String

data TransitionF b o u
  = Backable Event (Tree b o) u
  | ForwardOnly Event (Tree b o) u

type Transition b o u = Free (TransitionF b o) u

data TreeF i o b
  = TreeF (Lang b) (Transition b o ())
  | TreeF1 (i -> Lang b) (Transition b o ())

newtype Tree i o = Tree (Exists (TreeF i o))

type PartialTrans i o b = Transition b o () -> Tree i o
data Event' i o = Event' Event (Tree i o)

instance Functor (TransitionF b o) where
  fmap f (Backable e g next)    = Backable e g (f next)
  fmap f (ForwardOnly e g next) = ForwardOnly e g (f next)

(<~>) = backable''
(~>) = forwardOnly''

infixl 3 <~>
infixl 3 ~>

with :: forall b o. Lang b -> Transition b o () -> Tree () o
with flow table = Tree $ mkExists $ TreeF flow table

with1 :: forall i b o. (i -> Lang b) -> Transition b o () -> Tree i o
with1 flowF1 table = Tree $ mkExists $ TreeF1 flowF1 table

leaf :: Lang () -> Tree () ()
leaf flow = with flow (pure ())

leaf1 :: forall i. (i -> Lang ()) -> Tree i ()
leaf1 flowF1 = with1 flowF1 (pure ())

tree part = part $ pure ()

on = Event'

transable transType part (Event' e g) = part . transed
  where
    transed prevTrans = do
      prevTrans
      transType e g

backable' :: Event -> Tree i o -> Transition i o ()
backable' e g = liftF $ Backable e g ()

forwardOnly' :: Event -> Tree i o -> Transition i o ()
forwardOnly' e g = liftF $ ForwardOnly e g ()

backable'' = transable backable'
forwardOnly'' = transable forwardOnly'

---- Evaluation

data TrackResult a = BackTrack a | ForwardTrack a | Nop
data LangResult a b = Forward a b | Backward
data TransitionResult = Fallback | FallbackRerun | Done

interpretTransition :: forall b o u.
  Event -> TransitionF b o u -> State (TrackResult (Tree b o)) u
interpretTransition e (Backable expectedE g next) = do
  when (e == expectedE) (put $ BackTrack g)
  pure next
interpretTransition e (ForwardOnly expectedE g next) = do
  when (e == expectedE) (put $ ForwardTrack g)
  pure next

runTransition' :: forall b o s.
  Event -> Transition b o s -> State (TrackResult (Tree b o)) s
runTransition' e = foldFree (interpretTransition e)

runTransition :: forall i o b. Event -> TreeF i o b -> TrackResult (Tree b o)
runTransition e (TreeF _ t)  = execState (runTransition' e t) Nop
runTransition e (TreeF1 _ t) = execState (runTransition' e t) Nop
