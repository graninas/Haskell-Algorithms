module ParsingTest where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

data OpExpr = OpExpr (Char, Int) FPExpr
  deriving (Show)
  
data FPExpr = IntExpr FPExpr [OpExpr]
            | BrackedExpr FPExpr
            | Lit String
  deriving (Show)

integer = do
    i <- many1 (oneOf ['0'..'9'])
    return $ Lit i

op1 = (char '+', 5)
op2 = (char '*', 4)
op3 = (char '-', 5)

expr = do
    many space
    i1 <- integer <|> brackedExpr
    exprs <- many opExpr
    return $ IntExpr i1 exprs

op (p, priority) = do
    res <- p
    return (res, priority)
    
opExpr = do
    many space
    oper <- op op1 <|> op op2 <|> op op3
    many space
    e <- integer <|> brackedExpr
    return $ OpExpr oper e

brackedExpr :: GenParser Char st FPExpr
brackedExpr = do
    e <- between (char '(') (char ')') expr
    return $ BrackedExpr e
    
str1 = "3 + (2 - 1) * 5"
str2 = "3"
str3 = "3 + 5"
str4 = "(2 - 1) * 5 * 2"
str5 = "1 + 2 * 3"

test = do
    
    let result1 = parse expr "" str1
    let result2 = parse expr "" str2
    let result3 = parse expr "" str3
    let result4 = parse expr "" str4
    let result5 = parse expr "" str5
    
    
    print result1
    print result2
    print result3
    print result4
    print result5
    
    translate result5