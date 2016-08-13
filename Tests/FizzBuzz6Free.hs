{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

data FizzBuzz a = GetFizz Int Int (String -> a)
                | GetBuzz Int Int (String -> a)
                | GetNum  Int Int (String -> a)
--  deriving (Functor)
  
type FizzBuzzer a = Free FizzBuzz a

instance Functor FizzBuzz where
    fmap f (GetFizz n m next) = GetFizz n m (fmap f next)
    fmap f (GetBuzz n m next) = GetBuzz n m (fmap f next)
    fmap f (GetNum n m next)  = GetNum  n m (fmap f next)

getFizz, getBuzz, getNum :: Int -> Int -> FizzBuzzer String
getFizz n m = liftF (GetFizz n m id)
getBuzz n m = liftF (GetBuzz n m id)
getNum z n  = liftF (GetNum  z n id)


getFizz', getBuzz', getNum' :: Int -> Int -> FizzBuzzer String
getFizz' n m = Free (GetFizz n m Pure)
getBuzz' n m = Free (GetBuzz n m Pure)
getNum' z n  = Free (GetNum  z n Pure)

getFizzBuzz :: Int -> FizzBuzzer String
getFizzBuzz n = do
    fizz <- getFizz n 5
    buzz <- getBuzz n 3
    let fb = fizz ++ buzz
    s <- getNum (length fb) n
    return $ s ++ fizz ++ buzz
    
interpret :: FizzBuzzer String -> String
interpret (Pure a) = a
interpret (Free fb) = case fb of
    GetFizz n m next -> interpret $ next $ if n `mod` m == 0 then "Fizz" else ""
    GetBuzz n m next -> interpret $ next $ if n `mod` m == 0 then "Buzz" else ""
    GetNum 0 n next  -> interpret $ next $ show n
    GetNum _ _ next  -> interpret $ next $ ""

main = do
    let fizzBuzzes = map (interpret . getFizzBuzz) [1..15]
    mapM_ print fizzBuzzes
    