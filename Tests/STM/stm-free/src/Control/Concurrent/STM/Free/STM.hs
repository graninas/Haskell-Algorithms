{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STM where

import           Control.Monad.Free
import           Data.Aeson                       (FromJSON, ToJSON, decode,
                                                   encode)
import           GHC.Generics                     (Generic)

import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

data StmModelF next where
  Atomically :: STML a -> (a -> next) -> StmModelF next

instance Functor StmModelF where
  fmap g (Atomically stml nextF) = Atomically stml (g . nextF)

type STM next = Free StmModelF next

atomically :: STML a -> STM a
atomically l = liftF (Atomically l id)
