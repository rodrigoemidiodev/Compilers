{-
  Interpreter for a small first-order functional language
  Based on chap.5 of Basics of Compiler Design by Torben Mogensen
  Extension for imperative variables and control-flow

  Pedro Vasconcelos, 2021
 
-}
module Interpret2 where

import AST
import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad.State

data Value = IntVal Int | BoolVal Bool | Unit
           deriving (Eq, Show)

-- value environment (vtable in the book)
type ValEnv = Map Ident Loc

type Store = Map Loc Value

type Loc = Int

-- function Environment (ftable in the book)
type FunEnv = Map Ident Fundef

-- evaluate expressions
evalExpr :: ValEnv -> FunEnv -> Expr -> Store -> (Value, Store)
evalExpr vtabl ftabl (Num n) store 
  = (IntVal n, store)

evalExpr vtabl ftab  (Var x) store=
  case Map.lookup x vtabl of
      Nothing -> error "unknown variable"
      Just loc -> case Map.lookup loc store of
        Nothing -> error "unkown location"
        Just val -> (val, store)
     
evalExpr vtabl ftabl (Op binop e1 e2) store 
  = let (val1, store1) = evalExpr vtabl ftabl e1 store
        (val2, store2) = evalExpr vtabl ftabl e2 store1
    in (evalOp binop val1 val2, store2)


evalExpr vtabl ftabl (IfElse e1 e2 e3) store
  = let (val1, store1) = evalExpr vtabl ftabl e1 store
    in case val1 of
         BoolVal True -> evalExpr vtabl ftabl e2 store1
         BoolVal False -> evalExpr vtabl ftabl e3 store1
         _ -> error "type error in if then else"

evalExpr vtabl ftabl (Fun f exprs) store
  =let (vals, store1) = evalExprs vtabl ftabl exprs store
   in case Map.lookup f ftabl of
         Nothing -> error "undefined function"
         Just (Fundef _ args body) ->
           let (locs, store2) = allocValues vals store1
               vtabl' = bindArgs (zip args locs) vtabl
               (result, store3) = evalExpr vtabl' ftabl body store2
           in (result, deleteLocs locs store3)

     
evalExpr vtabl ftabl (Let x e1 e2) store
  = let (val1, store1) = evalExpr vtabl ftabl e1 store
        ([loc1], store2) = allocValues [val1] store1
        vtabl' = Map.insert x loc1 vtabl
        (val2, store3) = evalExpr vtabl' ftabl e2 store2
    in (val2, deleteLocs [loc1] store3)


       
evalExpr vtabl ftabl (Assign x e1) store
  = let (val1, store1) = evalExpr vtabl ftabl e1 store
    in case Map.lookup x vtabl of
         Nothing -> error "unkown variable"
         Just loc -> (Unit, Map.insert loc val1 store1)


evalExpr vtabl ftabl (Seq e1 e2) store
  = let (_, store1) = evalExpr vtabl ftabl e1 store
    in evalExpr vtabl ftabl e2 store1

evalExpr vtabl ftabl (While e1 e2) store
  = let (val1,store1) = evalExpr vtabl ftabl e1 store
    in case val1 of
         BoolVal False -> (Unit, store1)
         BoolVal True -> let (_, store2)= evalExpr vtabl ftabl e2 store1
                         in evalExpr vtabl ftabl (While e1 e2) store2
         _ -> error "type error in while condition"


evalExprs :: ValEnv -> FunEnv -> [Expr] -> Store -> ([Value], Store)
evalExprs vtabl ftabl [] store = ([], store)
evalExprs vtabl ftabl (first:rest) store
  = let (v1, store1) = evalExpr vtabl ftabl first store
        (vs, store2) = evalExprs vtabl ftabl rest store1
    in (v1:vs, store2)


bindArgs :: [(Ident,Loc)] -> ValEnv -> ValEnv
bindArgs [] vtabl = vtabl
bindArgs ((x,loc):rest) vtabl = bindArgs rest (Map.insert x loc vtabl) 


allocValues :: [Value] -> Store -> ([Loc], Store)
allocValues [] store = ([], store)
allocValues (val:vals) store
  = let newLoc = Map.size store
        store1 = Map.insert newLoc val store
        (newLocs, newStore) = allocValues vals store1
    in (newLoc:newLocs, newStore)
       
deleteLocs :: [Loc] -> Store -> Store
deleteLocs [] store = store
deleteLocs (loc:locs) store = deleteLocs locs (Map.delete loc store)



evalOp :: BinOp -> Value -> Value -> Value
evalOp Plus (IntVal a) (IntVal b) = IntVal (a+b)
evalOp Minus (IntVal a) (IntVal b) = IntVal (a-b)
evalOp Mult (IntVal a) (IntVal b)  = IntVal (a*b)
evalOp Mod (IntVal a) (IntVal b)  = IntVal (mod a b)

evalOp Lt (IntVal a) (IntVal b) = BoolVal (a<b)
evalOp Eq (IntVal a) (IntVal b) = BoolVal (a==b)
evalOp Neq (IntVal a) (IntVal b) = BoolVal (a/=b)
evalOp _  _           _    = error "not yet implemented"




-- evaluate a complete program
evalProg :: Prog ->  Value
evalProg (Prog defs main) =
  let ftabl =
        Map.fromList [(name, def) | def@(Fundef name vars body)<-defs]
  in fst (evalExpr Map.empty ftabl main Map.empty)
