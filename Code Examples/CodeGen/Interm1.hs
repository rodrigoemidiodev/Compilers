{-
  Basic translation of an imperative language
  into 3-addresss Intermediate code
  Version 1: Explicit passing of temporary and label counters
  Pedro Vasconcelos, 2022
-}
module Interm1 where

import AST
import IR
import           Data.Map (Map)
import qualified Data.Map as Map

-- symbol table mapping variables to temporaries
type Table = Map Ident Temp

-- translate an expression
transExpr :: Table -> Expr -> Ident -> Count -> ([Instr], Count)
transExpr tabl (Var x) dest c0
  = case Map.lookup x tabl of
      Just temp -> ([MOVE dest temp], c0)
      Nothing -> error "invalid variable"

transExpr tabl (Num n) dest c0
  = ([MOVEI dest n], c0)

transExpr tabl (Op op e1 e2) dest c0
  = let (temp1, c1) = newTemp c0
        (temp2, c2) = newTemp c1
        (code1, c3) = transExpr tabl e1 temp1 c2
        (code2, c4) = transExpr tabl e2 temp2 c3
        code = code1 ++ code2 ++ [OP op dest temp1 temp2]
    in (code, c4)


transExpr tabl (Fun f args) dest c0
  = case Map.lookup f tabl of
      Nothing  -> error "undefined function"
      Just flabel -> let (code, temps, c1) = transArgs tabl args c0
                     in (code ++ [CALL dest flabel temps], c1)

transArgs tabl args c0 = worker args c0
  where
    worker [] c0 = ([], [], c0)
    worker (exp:exps) c0
      = let (temp, c1) = newTemp c0
            (code, c2) = transExpr tabl exp temp c1
            (code', temps, c3) = worker exps c2
        in (code++code', temp:temps, c3)


transStm :: Table -> Stm -> Count -> ([Instr], Count)
transStm tabl (Assign var expr) c0
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just loc -> let (temp, c1) = newTemp c0     
                      (code, c2) = transExpr tabl expr temp c1
                  in (code ++ [MOVE loc temp], c2)

transStm tabl (If cond stm1) c0
  = let (ltrue, c1) = newLabel c0
        (lfalse, c2)= newLabel c1
        (code0, c3) = transCond tabl cond ltrue lfalse c2
        (code1, c4) = transStm tabl stm1 c3
        code = code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse]
    in (code, c4)

               
transStm tabl (IfElse cond stm1 stm2) c0
  = let (ltrue, c1) = newLabel c0
        (lfalse, c2) = newLabel c1
        (lend, c3) = newLabel c2
        (code1, c4) = transStm tabl stm1 c3
        (code2, c5) = transStm tabl stm2 c4
        (code0, c6) = transCond tabl cond ltrue lfalse c4
        code = code0 ++ [LABEL ltrue] ++
               code1 ++ [JUMP lend, LABEL lfalse] ++
               code2 ++ [LABEL lend]
    in (code, c6)



transStm tabl (While cond stm) c0 =
  let (labeli, c1) = newLabel c0
      (labelt, c2) = newLabel c1
      (labelf, c3) = newLabel c2
      (code1, c4) = transCond tabl cond labelt labelf c3
      (code2, c5) = transStm tabl stm c4
      code = [LABEL labeli] ++ code1 ++ [LABEL labelt] ++
             code2 ++ [JUMP labeli, LABEL labelf]
  in (code, c5)


transStm tabl (Block []) c0 = ([], c0)
transStm tabl (Block (stm:stms)) c0 =
  let (code1,c1)= transStm tabl stm c0
      (code2,c2) = transStm tabl (Block stms) c1
  in (code1++code2, c2)


transCond :: Table -> Expr -> Label -> Label -> Count -> ([Instr], Count)
transCond tabl (Op rel e1 e2) ltrue lfalse c0
  | rel == Lt || rel == Lteq || rel == Eq || rel == Neq =
      let (t1,c1) = newTemp c0
          (t2,c2) = newTemp c1
          (code1,c3) = transExpr tabl e1 t1 c2
          (code2,c4) = transExpr tabl e2 t2 c3
          code = code1 ++ code2 ++ [COND t1 rel t2 ltrue lfalse]
      in (code, c4)


----------------------------------------------------------------------------------

type Count = (Int,Int)  -- contadores para temporários e etiquetas

newTemp :: Count -> (Ident, Count)
newTemp (t,l) = ("t"++show t, (t+1,l))

newLabel :: Count -> (Label, Count)
newLabel (t,l) = ("L"++show l, (t,l+1))
---------------------------------------------------------------------------
