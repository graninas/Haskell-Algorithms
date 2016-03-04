module Interpreter where

import Language

data Result = StrResult String
            | IntResult Int
            | CharResult Char
            | OperationResult Operation
            | SequenceResult Result Result
            | Eval (IO ())

evalMath Increment    (IntResult i) = IntResult $ i + 1
evalMath Decrement    (IntResult i) = IntResult $ i - 1
evalMath (Multiply n) (IntResult i) = IntResult $ i * n
evalMath m _ = Eval $ putStr $ "Invalid argument for math " ++ show m

evalGetChar (IntResult i) (StrResult str) | length str > i = CharResult (str !! i)
                                          | otherwise      = Eval $ putStr $ "Index out of bounds: " ++ show i
evalGetChar _ _ = Eval $ putStr "Invalid GetChar arguments."

evalSCombinator (OperationResult op) r = interpret op r
evalSCombinator _ _ = Eval $ putStr "Invalid operation for S combinator."

-- | Interpreter
interpret NoOperation _ = Eval $ putStrLn "No operation."
interpret GetLength (StrResult str) = IntResult (length str)
interpret GetLength (CharResult _)  = Eval (putStr "Invalid operation: can't get length of char value.")
interpret GetLength (IntResult i)   = Eval (putStr "Invalid operation: can't get length of int value.")
interpret GetLength (Eval _)        = Eval (putStr "Invalid operation: can't get length of this value.")
interpret GetLength (OperationResult _)  = Eval (putStr "Invalid operation: can't get length of Operation value.")
interpret GetLength (SequenceResult _ _) = Eval (putStr "Invalid operation: can't get length of Sequence value.")

interpret (GetCharAt op) d@(StrResult str) = let
    idx = interpret op d
    in evalGetChar idx d
interpret (GetCharAt _)  (IntResult i)   = Eval $ putStr "Invalid operation: can't extract char of int value."
interpret (GetCharAt _)  (CharResult i)  = Eval $ putStr "Invalid operation: can't extract char of char value."
interpret (GetCharAt _)  (Eval _)        = Eval $ putStr "Invalid operation: can't extract char of this value."
interpret (GetCharAt _)  (OperationResult _)  = Eval $ putStr "Invalid operation: can't extract char of Operation value."
interpret (GetCharAt _)  (SequenceResult _ _) = Eval $ putStr "Invalid operation: can't extract char of Sequence value."

interpret (ReturnString str) _ = StrResult str

interpret (Sequence op1 op2) d = let
    res1 = interpret op1 d
    res2 = interpret op2 d
    in SequenceResult res1 res2

interpret (ApplyMath math op) d = let
    x = interpret op d
    in evalMath math x

interpret (Apply op2 op1) d = let
    res = interpret op1 d
    in interpret op2 res

interpret (K op) _ = OperationResult op
interpret  I d     = d
interpret (S op1 op2) d = let
    h = interpret op1 d
    y = interpret op2 d
    in evalSCombinator h y

-- | Result evaluator
eval (Eval act)     = act
eval (IntResult i)  = putStr $ show i
eval (CharResult c) = putStr $ show c
eval (StrResult s)  = putStr s
eval (OperationResult op)  = putStr $ "Operation result is not finished: " ++ show op
eval (SequenceResult r1 r2) = eval r1 >> eval r2
