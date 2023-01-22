{-
 -------------------------------------------------------------
  Example implementation of the LR parsing algorithm in Haskell
  Pedro Vasconcelos, 2022
 -------------------------------------------------------------
-}
module LR where

import           Data.Map (Map, (!))
import qualified Data.Map as Map

-- Terminal and Non-terminal symbols are charaters
type Sym = Char

-- States are integers
type State = Int

-- The input is a string (of terminal symbols)
type Input = String

-- The 4 possible actions:
data Action = Shift State       -- push terminal and goto a state
            | Reduce Sym [Sym]  -- reduce by N -> alpha_1 ... alphaK
            | Go State          -- goto a state (after a reduce)
            | Accept            -- finish and accept
            deriving Show

-- The LR parsing table;
-- a mapping from pairs (state, symbol) to actions
type Table = Map (State,Sym) Action

-- A stack is a list of items;
-- the top of the stack is the first element
type Stack a = [a]

-- Configurations of the LR automaton
-- stack of states, stack of symbols, remaning input;
-- boths stacks operate in sync
--
type Conf = (Stack State, Stack Sym, Input)

--
-- Perform a single-step transition from a given configuration;
-- output is the new configuration
-- and also what action was taken 
--
step :: Table -> Conf -> (Conf, Action)
step tabl (states@(state:_), syms, input@(next:rest))
  = case Map.lookup (state,next) tabl of
      Nothing ->
        error ("no transition for " ++ show (state,next))
      Just (Shift state') ->
        ((state':states, next:syms, rest), Shift state')
      Just (Reduce lhs rhs) ->
        let n = length rhs
            states'@(top:_) = drop n states
            syms'           = drop n syms
            Go state' =  tabl!(top, lhs) -- must be of the form "goto n"
        in ((state':states', lhs:syms', input), Reduce lhs rhs)
      Just Accept ->
        ((states, syms, input), Accept)
      Just other ->
        error ("invalid entry for " ++ show (state,next))
        -- this shouldn't happen if the table is properly construted

--
-- Collect all transitions from an input string util an accept
-- or a runtime exception (reject)
--
transitions :: Table -> String -> [(Conf,Action)]
transitions tabl input = loop ([0],"",input)
  where
    loop conf = let (conf',action) = step tabl conf
                in (conf,action) : case action of
                                   Accept -> []
                                   _ -> loop conf'
  

--
-- Pretty-printing parsing steps given a table and input string
-- (1) prints only the current state (top of stack)
-- (2) reverses the stack of symbols (to print in usual order)
-- e.g. try:
-- printParsing example "aaabbccc$"
-- printParsing example "aabbccc$"
--
printParsing :: Table -> String -> IO ()
printParsing tabl input
  = sequence_ [ print (state, reverse syms, input, action )
              | ((state:_,syms,input), action) <- transitions tabl input
              ]




-------------------------------------------------------------------------
{-
  Example parsing table for the grammar

  T' -> T
  T -> R
  T -> aTc
  R -> epsilon
  R -> bR
-}
example :: Table
example =
  Map.fromList
  [ ((0,'a'), Shift 3),
    ((0,'b'), Shift 4),
    ((0,'c'), Reduce 'R' ""),
    ((0,'$'), Reduce 'R' ""),
    ((0,'T'), Go 1),
    ((0,'R'), Go 2),
    ((1,'$'), Accept),
    ((2,'c'), Reduce 'T' "R"),
    ((2,'$'), Reduce 'T' "R"),
    ((3,'a'), Shift 3),
    ((3,'b'), Shift 4),
    ((3,'c'), Reduce 'R' ""),
    ((3,'$'), Reduce 'R' ""),
    ((3,'T'), Go 5),
    ((3,'R'), Go 2),
    ((4,'b'), Shift 4),
    ((4,'c'), Reduce 'R' ""),
    ((4,'$'), Reduce 'R' ""),
    ((4,'R'), Go 6),
    ((5,'c'), Shift 7),
    ((6,'c'), Reduce 'R' "bR"),
    ((6,'$'), Reduce 'R' "bR"),
    ((7,'c'), Reduce 'T' "aTc"),
    ((7,'$'), Reduce 'T' "aTc")
  ]
