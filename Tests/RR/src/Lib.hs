{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad      (unless, when)
import           Control.Monad.Free
import           Data.UUID          (toString)
import           Data.UUID.V4       (nextRandom)

-- data FlowF next
--     = GenerateGUID
--         (Playback.RRItemDict Playback.GenerateGUIDEntry String)
--         (String -> next)
--
--     | RunIO (forall eff. BackendAff eff s) (s -> next)
--
--     | Log String s
--         (Playback.RRItemDict Playback.LogEntry UnitEx)
--         (UnitEx -> next)

data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next

instance Functor FlowF where
  fmap f (GenerateGUID next) = GenerateGUID (f . next)
  fmap f (RunIO ioAct next)  = RunIO ioAct (f . next)
  fmap f (LogInfo msg next)  = LogInfo msg (f . next)

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

logInfo :: String -> Flow ()
logInfo msg = liftF $ LogInfo msg id


compareGUIDs :: String -> Flow ()
compareGUIDs fileName = do
  newGuid <- generateGUID
  oldGuid <- runIO $ readFile fileName

  let equal = newGuid == oldGuid
  when equal $ logInfo "GUIDs are equal."
  unless equal $ logInfo "GUIDs are not equal."

interpretFlowF :: FlowF a -> IO a
interpretFlowF (GenerateGUID next) = next . toString <$> nextRandom
interpretFlowF (RunIO ioAct next)  = next <$> ioAct
interpretFlowF (LogInfo msg next)  = next <$> putStrLn msg


interpretFlowFTest :: FlowF a -> IO a
interpretFlowFTest (GenerateGUID next) = pure $ next "111"
interpretFlowFTest (RunIO ioAct next)  = error "IO not supported in tests."
interpretFlowFTest (LogInfo msg next)  = pure $ next ()



someFunc :: IO ()
someFunc = putStrLn "someFunc"
