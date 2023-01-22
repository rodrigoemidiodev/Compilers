{-
 Abstract syntax for a small first-order functional language
 Pedro Vasconcelos, 2021
-}
module AST where

type Ident = String

--- expressions
data Expr = Var Ident             -- x, y, z
          | Num Int               -- 1, 2, 3
          | Op BinOp Expr Expr    -- e1+e2, e1*e2, e1<e2, etc.
          | Fun Ident [Expr]      -- f(e1,...en)
          | IfElse Expr Expr Expr -- if then else
          | Let Ident Expr Expr   -- let x=e1 in e2
          | Assign Ident Expr     -- x := e
          | Seq Expr Expr         -- e1; e2
          | While Expr Expr       -- while e1 do e2
          deriving Show

-- operators
data BinOp = Plus | Minus | Mult | Div | Mod | Lt | Eq | Neq
           deriving (Eq, Show)

-- function definitions
data Fundef = Fundef Ident [Ident] Expr  -- f(x1,..,xn) = expr
            deriving Show

-- programs
data Prog = Prog [Fundef] Expr -- let f1=... fn=.. in e
            deriving Show






