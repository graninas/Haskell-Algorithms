module Types where

import qualified Data.Map as M

(=|>) :: Monad m => m a -> m a -> [m a]
(=|>) = undefined
(~->) :: Monad m => String -> m a -> m a
(~->) = undefined

infixl 0 =|>
infixl 1 ~->

setState :: Monad m => String -> m a
setState = undefined

delProp  = undefined

mapSubObjects  = undefined
enableProperty = undefined

noP = undefined

getProperty = undefined

prop :: Monad m => String -> String -> m a
prop = case getProperty "lockable" of
		"locked" -> \x -> case x of
						"unlock" -> setState "unlocked"
						"break"  -> delProp "lockable" >>= mapSubObjects enableProperty
		"unlocked" -> \_ -> noP


prop' :: Monad m => [m a]
prop' = case getProperty "lockable" of
		"locked" -> "unlock" ~-> setState "unlocked" =|>
					"break" ~->  delProp "lockable" >>= mapSubObjects enableProperty
		"unlocked" -> noP