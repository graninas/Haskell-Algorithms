module Main where

import Control.Monad.State
import Control.Monad
import System.Random

type Name = String
data Context = Context { ctxNextId :: State Context Int
                       , ctxNextName :: State Context Name }

getNextId = get >>= ctxNextId
getNextName = get >>= ctxNextName


-- The state, which will be injected into client code.
-- You can use nextId to provide sequental int numbers.
nextId :: Int -> State Context Int
nextId prevId = do let nId = prevId + 1
                   modify (\ctx -> ctx { ctxNextId = nextId nId })
                   return nId

-- Or, instead of nextId, you can use nextRnd to provide random int numbers.
nextRnd :: StdGen -> State Context Int
nextRnd prevG = do let (r, g) = randomR (0, 100) prevG
                   modify (\ctx -> ctx { ctxNextId = nextRnd g })
                   return r

-- And you can use nextName to provide some names.
nextName :: Int -> State Context Name
nextName 0 = do
    modify (\ctx -> ctx { ctxNextName = nextName 1 } )
    return "GNVOERK"
nextName 1 = do
    modify (\ctx -> ctx { ctxNextName = nextName 2 } )
    return "RIKTIG YOGLA"
nextName _ = return "BLABLABLATOR"


-- Client code. Knows nothing about furniture materials, but uses external state to create it.
type IkeaFurniture = (Int, Name)

createFurniture :: State Context IkeaFurniture
createFurniture = liftM2 (,) getNextId getNextName

ikea :: State Context [IkeaFurniture]
ikea = do
    table <- createFurniture
    shelf <- createFurniture
    return [table, shelf]
    
main :: IO ()
main = do
    print "Sequental ids:"
    print $ evalState ikea (Context (nextId 0) (nextName 0))
    
    print "Random ids:"
    print $ evalState ikea (Context (nextRnd (mkStdGen 100)) (nextName 0))
    
