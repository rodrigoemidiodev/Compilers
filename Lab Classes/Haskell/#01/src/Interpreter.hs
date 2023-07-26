{-
  Exercícios para a Aula Laboratorial 1

  Escrever um interpretador em Haskell da sintaxe abstrata de
  programas sequenciais.

  Pedro Vasconcelos, 2022.

  Baseado num exercício do livro "Modern Compiler Implementation in
  ML", A. Appel.
-}

module Interpreter where

--
-- sintaxe abstrata de programas sequenciais
--
type Ident = String  -- identificadores (nomes de variaveis)

data BinOp = Plus | Minus | Times | Div -- operações binárias
           deriving (Eq, Show)

data Stm = AssignStm Ident Exp   -- ident = exp
         | IncrStm Ident         -- ident++
         | CompoundStm Stm Stm   -- stm1; stm2
         deriving (Eq, Show)

data Exp = IdExp Ident           -- x, y, z ..
         | NumExp Int            -- 123
         | OpExp Exp BinOp Exp   -- e1+e2, e1*e2, ...
         deriving (Eq, Show)


{- Exercício 1.

Escrever duas funções recursivas para listar todos os identificadores
em comandos e expressões.

NOTA: escreva uma equação para cada construtor da sintaxe abstrata
acima. A função idsStm deve chamar idsExpr os comandos contêm sub-expressões.
-}

idsStm :: Stm -> [Ident]
idsStm (AssignStm var exp) = var : idsExp exp
idsStm (IncrStm var) = [var]
idsStm (CompoundStm stm1 stm2) = idsStm stm1 ++ idsStm stm2 

idsExp :: Exp -> [Ident]
idsExp (IdExp var) = [var]
idsExp (NumExp val) = []
idsExp (OpExp exp1 op exp2) = idsExp exp1 ++ idsExp exp2 

-- NB: o que acontece se um identificador ocorrer mais do que uma vez?


{- Exercício 2: um interpretador funcional 

Escreva duas funções mutuamente recursivas para interpretar comandos
e expressões.

Represente tabelas associações de valores (inteiros) aos
identificadores como listas de pares.
Por exemplo, a lista [("x", 2), ("y", 0)] associa x -> 2, y -> 0.

Sugestões: use a função do prelúdio

lookup :: Eq a => a -> [(a,b)] -> Maybe b

para procurar o valor (se existir) associado a um identificador.
-}

type Table = [(Ident, Int)]    

interpStm :: Stm -> Table -> Table
interpStm (AssignStm var exp) tab = update var (interpExp exp tab) tab
interpStm (IncrStm var) tab = update var (interpExp (IdExp var) tab + 1) tab 
interpStm (CompoundStm stm1 stm2) tab = interpStm stm2 (interpStm stm1 tab)
                                        
interpExp :: Exp -> Table -> Int
interpExp (IdExp var) tab = case lookup var tab of
  Just x -> x
  Nothing -> error "Variable not found in table"
interpExp (NumExp val) tab = val
interpExp (OpExp exp1 op exp2) tab = case op of
  Plus -> interpExp exp1 tab + interpExp exp2 tab
  Minus -> interpExp exp1 tab - interpExp exp2 tab
  Times -> interpExp exp1 tab * interpExp exp2 tab
  Div -> interpExp exp1 tab `div` interpExp exp2 tab

update :: String -> Int -> Table -> Table
update var val tab = case lookup var tab of
                        Nothing -> (var, val) : tab
                        Just _ -> (var, val) : filter (\(i, _) -> i /= var) tab 




  
