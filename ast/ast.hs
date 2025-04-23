module Ast where

data Expr = Plus Expr Expr 
          | Minus Expr Expr 
          | Times Expr Expr 
          | Div Expr Expr
          | Literal Float
          deriving (Show)

eval :: Expr -> Float
eval (Literal n)     = n
eval (Plus e1 e2)    = eval e1 + eval e2
eval (Minus e1 e2)   = eval e1 - eval e2
eval (Times e1 e2)   = eval e1 * eval e2
eval (Div e1 e2)     = eval e1 / eval e2

eq :: Expr -> Expr -> Bool
eq e1 e2 = eval e1 == eval e2

-- Should eval to "5.0"
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5"
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))
