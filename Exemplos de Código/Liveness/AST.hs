
module AST where

type Ident = String

--- expressions
data Expr = Var Ident            -- x, y, z
          | Num Int              -- 1, 2, 3
          | Op BinOp Expr Expr   -- e1+e2, e1*e2, e1<e2, etc.
          | Fun Ident [Expr]     -- f(e1,...en)
          deriving Show

data BinOp = Plus | Minus | Mult | Div | Mod | Lt | Lteq | Eq | Neq
           deriving (Eq, Show)

--- statements
data Stm = Assign Ident Expr
         | If Expr Stm
         | IfElse Expr Stm Stm
         | While Expr Stm
         | Block [Stm]
         | Return Expr
         deriving Show

example1
  = Op Plus (Num 1) (Op Mult (Num 2) (Num 3))

example2
  = IfElse
    (Op Lt (Var "x") (Num 0))
    (Assign "y" (Num 1))
    (Assign "y" (Num 2))

example3
  = While (Op Neq (Var "b") (Num 0))
    ( Block [ Assign "r" (Op Mod (Var "a") (Var "b"))
            , Assign "a" (Var "b")
            , Assign "b" (Var "r")
            ]
    )
