{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Interpreter where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

import           TransitionGraph.Lang
import           TransitionGraph.Graph

data TrackResult a = BackTrack a | ForwardTrack a | Nop
data LangResult a b = Forward a b | Backward
data TransitionResult = Fallback | FallbackRerun | Done

interpretTransition :: forall b o u.
  Event -> TransitionF b o u -> State (TrackResult (Graph b o)) u
interpretTransition e (Backable expectedE g next) = do
  when (e == expectedE) (put $ BackTrack g)
  pure next
interpretTransition e (ForwardOnly expectedE g next) = do
  when (e == expectedE) (put $ ForwardTrack g)
  pure next

runTransition' :: forall b o s.
  Event -> Transition b o s -> State (TrackResult (Graph b o)) s
runTransition' e = foldFree (interpretTransition e)

runTransition :: forall i o b. Event -> GraphF i o b -> TrackResult (Graph b o)
runTransition e (GraphF _ t)  = execState (runTransition' e t) Nop
runTransition e (GraphF1 _ t) = execState (runTransition' e t) Nop
