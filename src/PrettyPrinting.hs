module PrettyPrinting (prettyShowExpr) where

import Expression

data PrettyExpr v = PrettyExpr { prec :: Int, expr :: Expr v }

instance Show v => Show (PrettyExpr v) where
    show (PrettyExpr _ Zero) = "0"
    show (PrettyExpr _ One) = "1"
    show (PrettyExpr _ (Var v)) = show v
    show (PrettyExpr prec (Negative e)) = printUnOp prec '-' e
    show (PrettyExpr prec (lhs :+: Negative rhs)) = printBinOp prec '-' lhs rhs
    show (PrettyExpr prec (lhs :+: rhs)) = printBinOp prec '+' lhs rhs
    show (PrettyExpr prec (lhs :*: rhs)) = printBinOp prec '*' lhs rhs
    show (PrettyExpr prec (lhs :/: rhs)) = printBinOp prec '/' lhs rhs

prettyShowExpr :: Show v => Expr v -> String
prettyShowExpr = show . PrettyExpr 0

printBinOp prec op lhs rhs = if prec > opPrec then "(" ++ s ++ ")" else s
    where s = show (PrettyExpr opPrec lhs) ++ [' ', op,  ' '] ++ show (PrettyExpr opPrec rhs)
          opPrec = binOpPrec op

printUnOp prec op operand = if prec > opPrec then "(" ++ s ++ ")" else s
    where s = [op] ++ show (PrettyExpr opPrec operand)
          opPrec = unOpPrec op

binOpPrec op = case op of
                 '+' -> 1
                 '-' -> 1
                 '*' -> 2
                 '/' -> 2
                 _   -> 10 -- Just make sure it's higher than eny other precedence level

unOpPrec op = case op of
                '-' -> 3
                _   -> 10
