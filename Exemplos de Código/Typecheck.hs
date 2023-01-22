{-
 Typechecker for simple C-like imperative language
 Pedro Vasconcelos, 2022
-}
module Typecheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

-- type environment (i.e. symbol table)
type TypeEnv = Map Ident Type

----------------------------------------------------------------------------------
-- Expressions


checkExpr :: TypeEnv -> Expr -> Type
checkExpr env (Num n) = TyInt
checkExpr env (Var x) = case Map.lookup x env of
    Nothing -> error "undeclared variable"
    Just t -> t
checkExpr env (Add e1 e2) 
   = let t1 = checkExpr env e1 
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyInt
         else error "type error in +"
checkExpr env (LessThan e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in <"
checkExpr env (FunCall f args)
  = let ts = map (checkExpr env) args
    in case Map.lookup f env of
         Just (TyFun ts' t) ->
           if ts == ts' then t
           else error "type error in application"
         _ -> error "invalid function name"

-------------------------------------------------------------------------------------
-- Statements

checkStm :: TypeEnv -> Maybe Type -> Stm -> Bool
checkStm env tyret (Assign var expr)
  = case Map.lookup var env of
      Nothing -> error "undeclared variable"
      Just t1 -> let t2 = checkExpr env expr
                 in if t1 == t2  then True
                    else error "type error in assignment"
checkStm env tyret (IfThenElse cond stm1 stm2)
  = let t0 = checkExpr env cond                           
    in if t0 == TyBool then
         checkStm env tyret stm1 &&
         checkStm env tyret stm2
       else error "type error: condition should be bool"

checkStm env tyret (IfThen cond stm)
  = let t0 = checkExpr env cond                           
    in if t0 == TyBool then
         checkStm env tyret stm 
       else error "type error: condition should be bool"

checkStm env tyret (While exp stm1)
  = let t0 = checkExpr env exp
    in if t0 == TyBool then checkStm env tyret stm1
       else error "type error: condition should be bool"

checkStm env tyret (Block decls stms)
  = let env' = extendEnv env decls
    in all (checkStm env' tyret) stms

checkStm env (Just typ) (Return exp)
  = checkExpr env exp == typ
checkStm env Nothing (Return _)
  = error "type error: return outside of function"

extendEnv :: TypeEnv -> [Decl] -> TypeEnv
extendEnv env [] = env
extendEnv env ((v,t):rest) = extendEnv (Map.insert v t env) rest

checkFunDef :: TypeEnv -> FunDef -> TypeEnv
checkFunDef env (FunDef fun decls tyret stm)
  = let tyargs = map snd decls
        env' = extendEnv env ((fun, TyFun tyargs tyret):decls)        
    in if checkStm env' (Just tyret) stm then
         extendEnv env [(fun,TyFun tyargs tyret)] 
       else error "type error in function definition"

-- check a program
-- sintetize a type environment from all function declarations
-- check the program body using that environment
checkProg :: Prog -> Bool
checkProg (Prog defs stm)
  = let env = foldl checkFunDef Map.empty defs
    in checkStm env Nothing stm

