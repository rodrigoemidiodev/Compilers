{-
  Compute NULLABLE, FIRST and FOLLOW mappings from
  a Context-free Grammar

  Pedro Vasconcelos, 2021, 2022
-}
module Grammar where

import           Data.List
import           Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Map (Map, (!))

-- Both terminal and non-terminal symbols are represented by strings
type Sym = String

-- A rule is a pair of (lhs, rhs)
-- lhs is a (non-terminal) symbol;
-- rhs is a list of symbols (possibly empty)
type Rule = (Sym, [Sym])

-- A grammar is a list of rules;
-- the first rule should be for the initial symbol
type Grammar = [Rule]

-- Example grammar
example1 = [ ("S'", ["S", "$"])
            , ("S", ["A", "B"])
            , ("A", ["a", "A", "b"])
            , ("A", [])
            , ("B", ["b", "B"])
            , ("B", [])
            ]

-- Grammar for expressions
example2 = [ ("Expr", ["Term", "TermList"])
          , ("TermList", ["+", "Term", "TermList"])
          , ("TermList", [])
          , ("Term", ["Factor", "FactorList"])
          , ("FactorList", ["*", "Factor", "FactorList"])
          , ("FactorList", [])
          , ("Factor", ["num"])
          , ("Factor", ["(", "Expr", ")"])
          ]


type Nullable = Map Sym Bool
type First = Map Sym (Set Sym)
type Follow = Map Sym (Set Sym)


nonTerminals :: Grammar -> [Sym]
nonTerminals = nub . map fst

isNonTerminal :: Sym -> Bool
isNonTerminal (x:_) = isUpper x

-- check if a sequence of symbols is nullable
-- given the nullable mapping for non-terminals
nullableSyms :: Nullable -> [Sym] -> Bool
nullableSyms nullable xs
  = and [isNonTerminal x && nullable!x | x<-xs]


-- compute the FIRST terminals of a sequence of symbols
-- given nullable and first mappings for non-terminals
firstSyms :: Nullable -> First -> [Sym] -> Set Sym
firstSyms nullable first xs = go xs
  where go [] = Set.empty
        go (x:xs) 
          | isNonTerminal x = (first!x) `Set.union`
                              if nullable!x then go xs else Set.empty
          | otherwise = Set.singleton x


-- compute NULLABLE mapping
findNullable :: Grammar -> Nullable
findNullable rules = fixpoint next initial
  where
    vars = nonTerminals rules
    initial = Map.fromList $ zip vars (repeat False)
    next iter = Map.fromListWith
                (||)
                [ (var, nullableSyms iter rhs) | (var,rhs) <- rules ]


-- compute FIRST mapping
findFirst :: Grammar  -> First
findFirst rules = fixpoint next initial
  where
    nullable = findNullable rules
    vars = nonTerminals rules
    initial = Map.fromList $ zip vars (repeat Set.empty)
    next iter = Map.fromListWith Set.union
                [ (var, firstSyms nullable iter rhs) | (var, rhs)<-rules]



-- compute FOLLOW mapping
findFollow :: Grammar -> Follow
findFollow rules = fixpoint next initial 
  where
    vars = nonTerminals rules
    nullable = findNullable rules
    first = findFirst rules
    initial = Map.fromList $ zip vars (repeat Set.empty)
    next iter = Map.fromListWith Set.union
                [ (x,s) | (var, rhs) <- rules
                        , (x:rest) <- tails rhs
                        , isNonTerminal x
                        , s <- [firstSyms nullable first rest] ++
                               [iter!var | nullableSyms nullable rest]
                        ] 
               

-- compute iterations until a fixed point is reached
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint transf initial = go initial
  where
    go x = let x' = transf x
           in if x==x' then x
              else go x'

