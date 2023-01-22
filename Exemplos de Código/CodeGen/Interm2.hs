{-
  Basic Translation of imperative language
  into 3-addresss Intermediate code
  Version 2: using a state monad
  Pedro Vasconcelosm, 2022
-}
module Interm2 where

import AST
import IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State


type Table = Map Ident String

-- translate an expression
transExpr :: Table -> Expr -> Temp -> State Count [Instr]
transExpr tabl (Var x) dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExpr tabl (Num n) dest 
  = return [MOVEI dest n]

transExpr tabl (Op op e1 e2) dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr tabl e1 temp1 
       code2 <- transExpr tabl e2 temp2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])


transExpr tabl (Fun f args) dest 
  = case Map.lookup f tabl of
      Nothing  -> error "undefined function"
      Just flabel -> do (code, temps) <- transArgs tabl args 
                        return (code ++ [CALL dest flabel temps])

transArgs tabl args = worker args 
  where
    worker [] = return ([], [])
    worker (exp:exps)
      = do temp <- newTemp 
           code <- transExpr tabl exp temp
           (code', temps) <- worker exps 
           return (code++code', temp:temps)


transStm :: Table -> Stm -> State Count [Instr]
transStm tabl (Assign var expr) 
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> do temp <- newTemp 
                      code <- transExpr tabl expr temp 
                      return (code ++ [MOVE dest temp])
                      

transStm tabl (If cond stm1) 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond tabl cond ltrue lfalse 
       code1  <- transStm tabl stm1
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])


transStm tabl (IfElse cond stm1 stm2) 
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond tabl cond ltrue lfalse 
       code1 <- transStm tabl stm1 
       code2 <- transStm tabl stm2 
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transStm tabl (While cond stm) =
  do 
     lcond <- newLabel
     lbody <- newLabel
     lend <- newLabel
     code1 <- transCond tabl cond lbody lend
     code2 <- transStm tabl stm
     return ([LABEL lcond] ++ code1 ++
              [LABEL lbody] ++ code2 ++ [JUMP lcond,LABEL lend])


transStm tabl (Return expr) =
  do temp <- newTemp
     code <- transExpr tabl expr temp
     return (code ++ [RETURN temp])


transStm tabl (Block stms) = do
  list <- mapM (transStm tabl) stms
  return (concat list)

transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]
transCond tabl (Op rel e1 e2) ltrue lfalse 
  | rel == Lt || rel == Lteq || rel == Eq || rel == Neq =
      do temp1 <- newTemp 
         temp2 <- newTemp 
         code1 <- transExpr tabl e1 temp1
         code2 <- transExpr tabl e2 temp2
         return ( code1 ++ code2 ++
                  [COND temp1 rel temp2 ltrue lfalse] )

----------------------------------------------------------------------------------

type Count = (Int,Int)  -- contadores para temporários e etiquetas

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

newLabel :: State Count Label 
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)
---------------------------------------------------------------------------
