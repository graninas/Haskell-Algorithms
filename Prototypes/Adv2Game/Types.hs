module Types where

import qualified Data.Map as M


removeProperty = undefined

(|>) :: a -> a -> [a]
x |> y = x : [y]

(~->) :: a -> b -> (a, b)
x ~-> y = (x, y)

infixl 0 |>
infixl 1 ~->

get = undefined
put = undefined

setState :: Monad m => String -> m a
setState = undefined

delProp :: Monad m => String -> a -> m a
delProp = undefined

mapSubObjects  = undefined
enableProperty = undefined

noP = undefined

getProperty = undefined

prop :: Monad m => a -> [(String, m a)]
prop = \o -> case getProperty "lockable" o of
		"locked" -> "unlock" ~-> setState "unlocked" |>
					"break"  ~-> do
									delProp "lockable" o
									mapSubObjects enableProperty o
		"unlocked" -> []



lockable = undefined
locked = undefined
openable = undefined
closed = undefined

data Object a = Object
	{
		properties :: a
	}

myObject = Object {properties = [lockable locked, openable closed]}
