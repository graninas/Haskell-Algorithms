module Command (run, getInfo, Command(..), CommandHolder(..)) where

class Command a where
   run :: a -> IO ()
   getInfo :: a -> String

data CommandHolder a = CommandHolder (IO ())
