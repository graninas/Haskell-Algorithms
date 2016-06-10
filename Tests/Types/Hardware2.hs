{-# LANGUAGE FlexibleInstances #-}

module Hardware2 (rotate) where

import Command

data Rotate

instance Command (CommandHolder Rotate) where
  run (CommandHolder c) = c
  getInfo (CommandHolder c) = show $ "Rotate"
  
  
rotate :: CommandHolder Rotate
rotate = CommandHolder $ putStrLn "command Rotate"





