{-# LANGUAGE GADTs #-}

module STM.Free where

import           Control.Monad.Free
import           STM.TVar

data STMF next where
  NewTVar   ::           a -> (TVar a -> next) -> STMF next
  WriteTVar :: TVar a -> a -> next             -> STMF next
  ReadTVar  :: TVar a ->      (a -> next)      -> STMF next

instance Functor STMF where
  fmap g (NewTVar        a nextF) = NewTVar        a (g . nextF)
  fmap g (WriteTVar tvar a next ) = WriteTVar tvar a (g next)
  fmap g (ReadTVar  tvar   nextF) = ReadTVar  tvar   (g . nextF)

type STM a = Free STMF a

newTVar :: a -> Free STMF (TVar a)
newTVar a = liftF (NewTVar a id)

writeTVar :: TVar a -> a -> Free STMF ()
writeTVar tvar a = liftF (WriteTVar tvar a ())

readTVar :: TVar a -> Free STMF a
readTVar tvar = liftF (ReadTVar tvar id)
