{-
  Interpreter for a small first-order functional language
  Based on chap.5 of Basics of Compiler Design by Torben Mogensen
  Extension for I/O actions

  Pedro Vasconcelos, 2021
 
-}
module Interpret4 where

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
evalExpr :: ValEnv -> FunEnv -> Expr -> StateT Store IO Value
evalExpr vtabl ftabl (Num n)
  = return (IntVal n)

evalExpr vtabl ftab (Var x) = do
  store <- get
  case Map.lookup x vtabl of
      Nothing -> error "unknown variable"
      Just loc -> case Map.lookup loc store of
        Nothing -> error "unkown location"
        Just val -> return val
     
evalExpr vtabl ftabl (Op binop e1 e2)
  = do val1 <- evalExpr vtabl ftabl e1
       val2 <- evalExpr vtabl ftabl e2
       return $ evalOp binop val1 val2


evalExpr vtabl ftabl (IfElse e1 e2 e3)
  = do val1 <- evalExpr vtabl ftabl e1
       case val1 of
         BoolVal True -> evalExpr vtabl ftabl e2
         BoolVal False -> evalExpr vtabl ftabl e3
         _ -> error "type error in if then else"


evalExpr vtabl ftabl (Fun "printi" [expr])
  = do val <- evalExpr vtabl ftabl expr
       case val of
         IntVal n -> do lift (print n)
                        return Unit
         _ -> error "type error in printi"
         
evalExpr vtabl ftabl (Fun "scani" [])
  = do n <- lift getLine
       return (IntVal $ read n)

         
evalExpr vtabl ftabl (Fun f exprs)
  = do vals <- mapM (evalExpr vtabl ftabl) exprs
       case Map.lookup f ftabl of
         Nothing -> error "undefined function"
         Just (Fundef _ args body) ->
           do locs <- mapM alloc vals
              let vtabl' = bindArgs (zip args locs) vtabl
              result <- evalExpr vtabl' ftabl body
              mapM_ free locs
              return result


       
evalExpr vtabl ftabl (Let x e1 e2)
  = do val1 <- evalExpr vtabl ftabl e1
       loc1 <- alloc val1
       let vtabl' = Map.insert x loc1 vtabl
       val2 <- evalExpr vtabl' ftabl e2
       free loc1
       return val2

       
evalExpr vtabl ftabl (Assign x e1)
  = do val1 <- evalExpr vtabl ftabl e1
       case Map.lookup x vtabl of
         Nothing -> error "unkown variable"
         Just loc -> do update loc val1
                        return Unit


evalExpr vtabl ftabl (Seq e1 e2)
  = do evalExpr vtabl ftabl e1
       evalExpr vtabl ftabl e2

evalExpr vtabl ftabl (While e1 e2)
  = do val1 <- evalExpr vtabl ftabl e1
       case val1 of
         BoolVal False -> return Unit
         BoolVal True -> do evalExpr vtabl ftabl e2
                            evalExpr vtabl ftabl (While e1 e2)
         _ -> error "type error in while condition"


bindArgs :: [(Ident,Loc)] -> ValEnv -> ValEnv
bindArgs [] vtabl = vtabl
bindArgs ((x,loc):rest) vtabl = bindArgs rest (Map.insert x loc vtabl) 
       


evalOp :: BinOp -> Value -> Value -> Value
evalOp Plus (IntVal a) (IntVal b) = IntVal (a+b)
evalOp Minus (IntVal a) (IntVal b) = IntVal (a-b)
evalOp Mult (IntVal a) (IntVal b)  = IntVal (a*b)
evalOp Mod (IntVal a) (IntVal b)  = IntVal (mod a b)

evalOp Lt (IntVal a) (IntVal b) = BoolVal (a<b)
evalOp Eq (IntVal a) (IntVal b) = BoolVal (a==b)
evalOp Neq (IntVal a) (IntVal b) = BoolVal (a/=b)
evalOp _  _           _    = error "not yet implemented"


alloc :: Value -> StateT Store IO Loc
alloc val = do
  store <- get
  let loc = Map.size store
  put (Map.insert loc val store)
  return loc

free :: Loc -> StateT Store IO ()
free loc = do
  store <- get
  put (Map.delete loc store)

update :: Loc -> Value -> StateT Store IO ()
update loc val = do
  store <- get
  put (Map.insert loc val store)


-- evaluate a complete program
evalProg :: Prog ->  IO Value
evalProg (Prog defs main) =
  let ftabl =
        Map.fromList [(name, def) | def@(Fundef name vars body)<-defs]
  in evalStateT (evalExpr Map.empty ftabl main) Map.empty
