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

import           TransitionGraph.Graph

data TrackResult a    = BackTrack a | ForwardTrack a | Nop
data LangResult a b   = Forward a b | Backward
data TransitionResult = Fallback    | FallbackRerun | Done

interpretTransition
  :: Event
  -> TransitionF lang b o u
  -> State (TrackResult (Graph lang b o)) u
interpretTransition e (Backable expectedE g next) = do
  when (e == expectedE) (put $ BackTrack g)
  pure next
interpretTransition e (ForwardOnly expectedE g next) = do
  when (e == expectedE) (put $ ForwardTrack g)
  pure next

runTransition'
  :: Event
  -> Transition lang b o s
  -> State (TrackResult (Graph lang b o)) s
runTransition' e = foldFree (interpretTransition e)

runTransition
  :: Event
  -> GraphF lang i o b
  -> TrackResult (Graph lang b o)
runTransition e (GraphF _ t)  = execState (runTransition' e t) Nop
runTransition e (GraphF1 _ t) = execState (runTransition' e t) Nop
