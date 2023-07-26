{-
  Interpreter for a small first-order functional language
  Based on chap.5 of Basics of Compiler Design by Torben Mogensen
  Pedro Vasconcelos, 2021
-}
module Interpret1 where

import AST
import qualified Data.Map as Map
import           Data.Map (Map)

data Value = IntVal Int | BoolVal Bool
           deriving (Eq, Show)

-- value environment (vtable in the book)
type ValEnv = Map Ident Value


-- function Environment (ftable in the book)
type FunEnv = Map Ident Fundef


-- evaluate expressions
evalExpr :: ValEnv -> FunEnv -> Expr -> Value
evalExpr vtabl ftabl (Num n) = IntVal n

evalExpr vtabl ftab (Var x)
  = case Map.lookup x vtabl of
      Nothing -> error "unknown variable"
      Just val -> val
     
evalExpr vtabl ftabl (Op binop e1 e2)
  = let val1 = evalExpr vtabl ftabl e1
        val2 = evalExpr vtabl ftabl e2
    in evalOp binop val1 val2


evalExpr vtabl ftabl (IfElse e1 e2 e3)
  = let val1 = evalExpr vtabl ftabl e1
    in case val1 of
         BoolVal True -> evalExpr vtabl ftabl e2
         BoolVal False -> evalExpr vtabl ftabl e3
         _ -> error "type error in if then else"

evalExpr vtabl ftabl (Fun f exprs)
  = let vals = map (evalExpr vtabl ftabl) exprs
    in case Map.lookup f ftabl of
         Nothing -> error "undefined function"
         Just (Fundef _ args body) ->
           let vtabl' = bindArgs (zip args vals) vtabl
           in evalExpr vtabl' ftabl body
      
evalExpr vtabl ftabl (Let x e1 e2)
  = let v1 = evalExpr vtabl ftabl e1
        vtabl' = Map.insert x v1 vtabl
    in evalExpr vtabl' ftabl e2


bindArgs :: [(Ident,Value)] -> ValEnv -> ValEnv
bindArgs [] vtabl = vtabl
bindArgs ((x,v):rest) vtabl = bindArgs rest (Map.insert x v vtabl) 
       


evalOp :: BinOp -> Value -> Value -> Value
evalOp Plus (IntVal a) (IntVal b) = IntVal (a+b)
evalOp Minus (IntVal a) (IntVal b) = IntVal (a-b)
evalOp Mult (IntVal a) (IntVal b)  = IntVal (a*b)
evalOp Mod (IntVal a) (IntVal b) = IntVal (mod a b)

evalOp Lt (IntVal a) (IntVal b) = BoolVal (a<b)
evalOp Eq (IntVal a) (IntVal b) = BoolVal (a==b)
evalOp _  _           _    = error "not yet implemented"



-- evaluate a complete program
evalProg :: Prog -> Value
evalProg (Prog defs main) =
  let ftabl =
        Map.fromList [(name, def) | def@(Fundef name vars body)<-defs]
  in evalExpr Map.empty ftabl main
