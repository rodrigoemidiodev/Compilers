{-
  3-Addresss Intermediate code
  Pedro Vasconcelos, 2022
-}
module IR (Instr(..), BinOp(..), Temp, Label) where

import AST (BinOp(..))


type Temp  = String
type Label = String

data Instr = MOVE Temp Temp 
           | MOVEI Temp Int
           | OP BinOp Temp Temp Temp
           | OPI BinOp Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp BinOp Temp Label Label
           | CALL Temp Label [Temp]         -- t:=CALL f(temps)
           | RETURN Temp
           deriving (Eq, Show)

