{-# LANGUAGE FlexibleInstances #-}

module Hardware1 (start, stop) where

import Command

data Start
data Stop


instance Command (CommandHolder Start) where
   run (CommandHolder io) = io -- TODO: how to get rid of boilerplate?
   getInfo (CommandHolder c) = show $ "Start"


instance Command (CommandHolder Stop) where
   run (CommandHolder io) = io -- TODO: how to get rid of boilerplate?
   getInfo (CommandHolder c) = show $ "Stop"
  
start :: CommandHolder Start
start = CommandHolder $ putStrLn "command start"

stop :: CommandHolder Stop
stop = CommandHolder $ putStrLn "command stop"


