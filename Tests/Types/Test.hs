module Main where

import Command
import Hardware1
import Hardware2



main = do
    run start
    run stop
    print $ getInfo start
    print $ getInfo stop
    run rotate
    
    
    print ""
