module ParsingTest where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

data OpExpr = OpExpr Char FPExpr
  deriving (Show)
  
data FPExpr = IntExpr FPExpr [OpExpr]
            | BrackedExpr FPExpr
            | Lit String
  deriving (Show)

integer = do
    i <- many1 (oneOf ['0'..'9'])
    return $ Lit i

op1 = char '+'
op2 = char '*'
op3 = char '-'

expr = do
    many space
    i1 <- integer <|> brackedExpr
    exprs <- many opExpr
    return $ IntExpr i1 exprs

opExpr = do
    many space
    op <- op1 <|> op2 <|> op3
    many space
    e <- integer <|> brackedExpr
    return $ OpExpr op e

brackedExpr :: GenParser Char st FPExpr
brackedExpr = do
    e <- between (char '(') (char ')') expr
    return $ BrackedExpr e
    
str1 = "3 + (2 - 1) * 5"
str2 = "3"
str3 = "3 + 5"
str4 = "(2 - 1) * 5 * 2"


test = do
    
    let result1 = parse expr "" str1
    let result2 = parse expr "" str2
    let result3 = parse expr "" str3
    let result4 = parse expr "" str4
    
    
    print result1
    print result2
    print result3
    print result4