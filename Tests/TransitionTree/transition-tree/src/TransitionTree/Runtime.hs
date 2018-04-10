{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionTree.Runtime where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

import           TransitionTree.Lang
import           TransitionTree.Tree

data Runtime m = Runtime
  { runLang_     :: forall output. Lang output -> m (Event, output)
  , isBackEvent_ :: Event -> Bool
  }

runLang' :: forall m a. Monad m =>
  Runtime m -> Lang a -> m (LangResult Event a)
runLang' (Runtime runLang isBackEvent) flow = do
  (e, i) <- runLang flow
  if isBackEvent e
    then pure Backward
    else pure $ Forward e i

getLang :: forall i o b. i -> TreeF i o b -> Lang b
getLang _     (TreeF  flow  _) = flow
getLang input (TreeF1 flowF _) = flowF input

makeTransition' :: forall m i o b. Monad m =>
  Runtime m -> Bool -> i -> TreeF i o b -> m TransitionResult
makeTransition' runtime backable i3 g3 = do
  let f3 = getLang i3 g3
  transitionResult <- makeTransition runtime f3 g3
  case transitionResult of
    Fallback -> if backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"
    Done -> pure Done
    FallbackRerun -> makeTransition' runtime backable i3 g3

makeTransition :: forall m i o b. Monad m =>
  Runtime m -> Lang b -> TreeF i o b -> m TransitionResult
makeTransition runtime f2 g2 = do
  flowResult <- runLang' runtime f2
  case flowResult of
    Forward e2 i3 -> do
      let trackResult = runTransition e2 g2
      case trackResult of
        Nop -> pure Done
        BackTrack g3@(Tree g3Ex) -> runExists (makeTransition' runtime True i3) g3Ex
        ForwardTrack g3@(Tree g3Ex) -> runExists (makeTransition' runtime False i3) g3Ex
    Backward -> pure Fallback

runTree :: forall m. Monad m => Runtime m -> Tree () () -> m ()
runTree runtime (Tree ex) = do
  _ <- runExists (makeTransition' runtime False ()) ex
  pure ()
