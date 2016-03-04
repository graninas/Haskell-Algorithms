module Language where

data Math = Increment
          | Decrement
          | Multiply Int
  deriving (Show, Read)

data Operation = Apply Operation Operation
               | GetLength
               | ApplyMath Math Operation
               | GetCharAt Operation
               | NoOperation
               | Sequence Operation Operation
               | ReturnString String
               | K Operation
               | S Operation Operation
               | I
  deriving (Show, Read)
  
type Operations = [Operation]
  
-- Apply :: op1 -> op2 -> d -> res
-- Apply takes the second argument, applies to the third one,
-- then applies the first argument to the previous result. It acts like so:
-- Apply op2 op1 d = let
--    res1 = op1 d
--    res2 = op2 res
--    in res2

-- ApplyMath takes the second argument, applies to the third one,
-- then tries to eval math equation with the previous result. It acts like so:
-- ApplyMath :: Math -> op -> d -> res
-- ApplyMath math op d = let
--    res1 = op d
--    res2 = evalMath math res1
--    in res2

-- GetLength :: String -> Int

-- GetCharAt :: (String -> Int) -> String -> Char
-- GetCharAt takes 2 arguments: some function f and string.
-- GetCharAt applies f to string, which result is integer index.
-- Then GetCharAt extracts a char from that string at that index.

-- PrintMe :: String -> a -> b
-- PrintMe ignores it's second parameter and just prints some string.

-- ReturnString :: String -> a -> b
-- ReturnString ignores it's second parameter, but allowes the first argument
-- to use as string in computations.

-- Sequence :: (String -> a) -> (String -> b) -> String -> [a, b]
-- Sequence: applies both operations to the input parameter separately. Example:
-- (Sequence (PrintMe "Hello, ") (PrintMe "World!")) => "Hello, World!"

-- I just returns its argument as is. It acts like so:
--   I x = x
-- K takes 2 arguments: function and some argument.
-- K ignores its second argument and returns the first one. It acts like so:
--    K a b = a
-- S recives 3 arguments: a, b, x. It acts like so:
--    S a b x = let
--      h = a x
--      y = b x
--      result = h y
--      in result
-- The last result value is result of S.

-- S, K, I: http://en.wikipedia.org/wiki/SKI_combinator_calculus
