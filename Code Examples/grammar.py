# 
# Grammar.py  - Computer NULLABLE, FIRST and FOLLOW sets
#
# Pedro Vasconcelos, 2021, 2022

import copy
from typing import List, Dict, Set, Tuple

#
# Context Free Gramar are represented by lists of Rules
# Each rule is a pair of non-terminal (string) and list of symbols (strings)
#
Rule = Tuple[str, List[str]]

# Example grammar
#
example1 = [ ("S'", ["S", "$"])
          , ("S", ["A", "B"])
          , ("A", ["a", "A", "b"])
            , ("A", [])
            , ("B", ["b", "B"])
            , ("B", [])
            ]
            

# Grammar for expressions
#
example2 = [ ("Expr", ["Term", "TermList"])
             , ("TermList", ["+", "Term", "TermList"])
             , ("TermList", [])
             , ("Term", ["Factor", "FactorList"])
             , ("FactorList", ["*", "Factor", "FactorList"])
             , ("FactorList", [])
             , ("Factor", ["num"])
             , ("Factor", ["(", "Expr", ")"])
          ]

#
# NULLABLE, FIRST and FOLLOW are dictionaries
#
Nullable = Dict[str,bool]
First = Dict[str,Set[str]]
Follow = Dict[str,Set[str]]

# auxiliary definitions

def is_nonterminal(sym: str) -> bool:
    return sym[0].isupper()

def is_terminal(sym: str) -> bool:
    return not is_nonterminal(sym)

def non_terminals(rules: List[Rule]) -> Set[str]:
    return { var for (var,rhs) in rules }

# check if a sequence of symbols is nullable
# given the Nullable mapping for non-terminals
def nullable_syms(nullable: Nullable, xs: List[str]) -> bool:
    for x in xs:
        if is_terminal(x) or not nullable[x]:
            return False
    return True

# find the first terminal of a sequence
# given mappings for Nullable and First
def first_syms(nullable: Nullable, first: First, xs: List[str]) -> Set[str]:
    result = set()
    for x in xs:
        if is_nonterminal(x):
            result.update(first[x])
            if not nullable[x]:
                break
        else:
            result.add(x)
            break
    return result

#
# compute NULLABLE mapping
#
def find_nullable(rules: List[Rule]) -> Nullable:
    vars = non_terminals(rules)
    iter = { var:False for var in vars }
    prev = None
    while prev != iter:
        prev = iter.copy()  #  shallow copy can be used here
        for var,rhs in rules:
            iter[var] = iter[var] or nullable_syms(iter, rhs)
    return iter

#
# compute FIRST mapping
#
def find_first(rules: List[Rule]) -> First:
    nullable = find_nullable(rules)
    vars = non_terminals(rules)
    iter : First = { var:set() for var in vars }
    prev = None
    while prev != iter:
        prev = copy.deepcopy(iter) # because of dictionary values are sets
        for var,rhs in rules:
            iter[var].update(first_syms(nullable,iter,rhs))
    return iter

#
# compute FOLLLOW mapping
#
def find_follow(rules: List[Rule]) -> Follow:
    nullable = find_nullable(rules)
    first = find_first(rules)
    vars = non_terminals(rules)
    iter : Follow = { var:set() for var in vars }
    prev = None
    while prev != iter:
        prev = copy.deepcopy(iter) # because dictionary values are sets
        for var,rhs in rules:
            for i in range(len(rhs)):
                x = rhs[i]
                if is_nonterminal(x):
                    rest = rhs[i+1:]
                    iter[x].update(first_syms(nullable,first,rest))
                    if nullable_syms(nullable,rest):
                        iter[x].update(iter[var])
    return iter




