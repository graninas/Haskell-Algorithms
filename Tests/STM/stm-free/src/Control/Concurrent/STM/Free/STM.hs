{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STM where

import           Control.Monad.Free
import           Data.Aeson                                            (FromJSON,
                                                                        ToJSON,
                                                                        decode,
                                                                        encode)
import           GHC.Generics                                          (Generic)

import           Control.Concurrent.STM.Free.Internal.STM.Interpreter
import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

data StmModelL next where
  Atomically :: STML a -> (a -> next) -> StmModelL next

instance Functor StmModelL where
  fmap g (Atomically stml nextF) = Atomically stml (g . nextF)

type STM next = Free StmModelL next

atomically :: STML a -> STM a
atomically l = liftF (Atomically l id)
