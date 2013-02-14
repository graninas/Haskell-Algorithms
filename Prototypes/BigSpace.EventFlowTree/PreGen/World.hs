module PreGen.World where

import Types

preGenWorldTL :: TimeLine
preGenWorldTL = 
	(0, '_', "Time begin")
	: (10, '_', "SpaceShip from future!")
	: (15, '_', "SpaceShip was damaged.")
	: (20, '_', "SpaceShip sended back to the future.")
	: (100, 'S', "SpaceShip begin")
	: (zip3 [101..] "paceShip" $ repeat [])
	++ (
		  (150, 'r', "SpaceShip sended to past and returned.")
		: (200, '_', "Time end")
		: []
		)