{-# LANGUAGE GADTs #-}
module STM.Free where


data STMF a next
  = NewTVar            a (TVar a  -> next)
  | WriteTVar (TVar a) a next
  | ReadTVar  (TVar a)   (a -> next)

data Free method where
  Free :: method

data Pure a = Pure a

instance Functor (STMF a) where
  fmap f (NewTVar        a nextF) = NewTVar        a (f . nextF)
  fmap f (WriteTVar tvar a next)  = WriteTVar tvar a (f next)
  fmap f (ReadTVar  tvar nextF)   = ReadTVar  tvar   (f . nextF)

  -- instance Functor ComponentF where
  --   fmap f (SensorDef cd ci p next) = SensorDef cd ci p (f next)
