{-# LANGUAGE UndecidableInstances, 
             FunctionalDependencies, 
             MultiParamTypeClasses, 
             FlexibleInstances #-}
             
module Language where

import qualified Data.ConfigFile as CF
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad.State  as S
import Data.Either.Utils

newtype Configuration = Configuration CF.ConfigParser

{-

data Section = Section String
data Option = Option String
data Cfg = Cfg String String

sect = Section
opt = Option

(Section s) <| (Option o) = Cfg s o

getSect (Cfg s _) = s
getOpt  (Cfg _ o) = o

type CfgEvaluator m a = Cfg -> m Configuration a

getOption (Configuration cp) cfg = forceEither $ CF.get cp (getSect cfg) (getOpt cfg)

option cfg = do
    cp <- R.ask
    return $ getOption cp cfg

intOption = option :: CfgEvaluator Int
strOption = option :: CfgEvaluator String

evaluate cfg def = runMonad def cfg

-}

intOption = undefined


loadConfiguration fileName = do
    conf <- CF.readfile CF.emptyCP fileName
    let cp = forceEither conf
    return ( Configuration cp {CF.optionxform = id, CF.accessfunc = CF.interpolatingAccess 10} )


class (Monad m) => Abstract d m | m -> d where
    runAbstract :: m d -> String -> m a


instance Abstract 
evaluate def opt = do
    cfg <- loadConfiguration opt
    return $ R.runReader def cfg


instance (Error e, Monad m, MonadTask a m) => MonadTask a (ErrorT e m) where
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . runErrorT 
  watch  = lift . watch
  signal = lift . signal


screenDef = do
    sw <- intOption "screenWidth"
    sh <- intOption "screenHeight"
    cd <- intOption "colorDepth"
    return (sw, sh, cd)

main = do
    screenOpts <- runAbstract screenDef "Options.cfg"
    
    pretty <- R.runWriter (runAbstract screenDef "") screenOpts
    putStrLn pretty
    
    return ()
    