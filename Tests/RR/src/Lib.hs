{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad      (unless, when)
import           Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, encode, decode)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: String -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

data RunIOEntry = RunIOEntry
  { jsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data LogInfoEntry = LogInfoEntry
  { message :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance RRItem GenerateGUIDEntry String where
  toRecordingEntry rrItem idx = RecordingEntry idx "GenerateGUIDEntry"
    $ BS.unpack $ BSL.toStrict $ encode rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) =
    decode $ BSL.fromStrict $ BS.pack payload
  getTag _ = "GenerateGUIDEntry"
  parseRRItem (GenerateGUIDEntry guid) = Just guid


data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next

instance Functor FlowF where
  fmap f (GenerateGUID next) = GenerateGUID (f . next)
  fmap f (RunIO ioAct next)  = RunIO ioAct (f . next)
  fmap f (LogInfo msg next)  = LogInfo msg (f . next)

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
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

data RecorderRuntime = RecorderRuntime
data PlayerRuntime = PlayerRuntime

data Runtime
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime


type EntryName = String
type EntryPayload = String
type EntryIndex = Int
data RecordingEntry = RecordingEntry EntryIndex EntryName EntryPayload

-- type RecordingEntries = [RecordingEntry]
-- newtype Recording = Recording RecordingEntries

class (Eq rrItem, ToJSON rrItem, FromJSON rrItem)
  => RRItem rrItem native
   | rrItem -> native where
  toRecordingEntry   :: rrItem -> Int -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String
  parseRRItem        :: rrItem -> Maybe native


record = undefined
replay = undefined

withRunMode
  :: RRItem rrItem native
  => Runtime
  -> (native -> rrItem)
  -> IO native
  -> IO native
withRunMode RegularMode _ act = act
withRunMode (RecordingMode recorderRt) rrItem act
  = record recorderRt rrItem act
withRunMode (ReplayingMode playerRt) rrItem act
  = replay playerRt rrItem act

interpretFlowFRR :: Runtime -> FlowF a -> IO a
interpretFlowFRR rt (GenerateGUID next) = do
  res <- withRunMode rt
    mkGenerateGUIDEntry
    (toString <$> nextRandom)
  pure $ next res
interpretFlowFRR rt (RunIO ioAct next)  = error "IO not supported in tests."
interpretFlowFRR rt (LogInfo msg next)  = pure $ next ()

-- initDB :: String -> DB.DBConfig -> Maybe DB.Connection
-- initDB dbName cfg = do
--   mbConn <- runIO $ DB.initDatabase dbName cfg
--   when (isJust mbConn) $ logInfo $ "Successfully initialized."
--   pure mbConn

-- scenario :: Flow Int
-- scenario = do
--   students <- runDBQuery "SELECT * FROM students"
--   when (null students) $ logInfo "No records found."
--   pure $ length students

someFunc :: IO ()
someFunc = putStrLn "someFunc"
