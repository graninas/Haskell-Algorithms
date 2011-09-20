

data Object = Object
	{
		properties :: [Property]
	} deriving (Eq)


data Property = Property String [(State, String)]
	 deriving (Eq)

data State = State String
	 deriving (Eq)

switch (st1, act1) (st2, act2) x | x == st2 = [(st1, act1)]
								 | x == st1 = [(st2, act2)]


opened = State "opened"
closed = State "closed"

openable :: State -> Property
openable = Property "openable" . switch (opened, "open") (closed, "close")

drawer = Object { properties = [openable opened] }


propertyActions :: Property -> [String]
propertyActions (Property _ xs) = foldr (\(_, act) -> (act:)) [] xs

actions :: [Property] -> [String]
actions = foldr (\p -> (propertyActions p ++)) []


main :: IO ()
main = do
		putStrLn "test"
