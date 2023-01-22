{-
  Abstract Syntax Trees for a simple C-like imperative language
  Pedro Vasconcelos, 2022
-}
module AST where

type Ident = String

--- types
data Type = TyInt              -- integers
          | TyBool             -- booleans
          | TyFun [Type] Type  -- functions
          deriving (Show, Eq)

--- expressions
data Expr = Var Ident             -- x, y, z
          | Num Int               -- 1, 2, 3
          | Add Expr Expr         -- e1 + e2
          | LessThan Expr Expr    -- e1 < e2
          | FunCall Ident [Expr]  -- f(e1,e2,...)
          deriving Show

--- statements
data Stm = Assign Ident Expr        -- var = expr
         | IfThenElse Expr Stm Stm  -- if (cond) stm1 else stm2
         | IfThen Expr Stm          -- if (cond) stm
         | While Expr Stm           -- while (cond) stm
         | Block [Decl] [Stm]       -- { decls; stms }
         | Return Expr              -- return expr
         deriving Show

--- variable declarations
type Decl = (Ident, Type)           -- variable, type

--- function declarations
data FunDef
  = FunDef Ident [(Ident,Type)] Type Stm  
  deriving Show

--- complete programs
data Prog = Prog [FunDef] Stm
  deriving Show

--- examples:

example1 =
  Block
    [("x", TyInt), ("y",TyInt)]
    [ Assign "x" (Num 42)
    , Assign "y" (Num 77)
    , IfThen
      (LessThan (Var "x") (Var "y"))
      (Assign "y" (Var "x"))     
    ]

example2 =
  Block [("x", TyInt), ("y",TyInt)]
  [ Assign "x" (Num 42)
  , Assign "x" (Num 77)
  , IfThen (Var "x" `LessThan` Var "y")
    (Block [("temp", TyInt)]
     [ Assign "temp" (Var "y")
     , Assign "y" (Var "x")
     , Assign "x" (Var "temp")
     ])
  ]

example3 =
  FunDef "f" [("x", TyInt)] TyInt (Return (LessThan (Var "x") (Num 1)))

example4 =
  FunDef "f"
     [("x", TyInt)] TyInt
     (IfThenElse
      (LessThan (Var "x") (Num 0))
      (Return (Num 0))
      (Return (Add (Num 2) (FunCall "f" [Add (Var "x") (Num (-1))])))
     )
