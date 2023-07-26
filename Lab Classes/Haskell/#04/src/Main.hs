{-
  Uma calculadora simples em Haskell
  ----------------------------------

  Completar:
  - o analisador lexical no ficheiro Lexer.x;
  - o analisador sintático no ficheiro Parser.y

  O analisador sintático devolve diretamente o valor da expressão
  (Float) sem construir uma árvore sintática.

  Este módulo apenas lê uma linha de input e invoca
  o analisador lexical e sintático em sequência.

  Pedro Vasconcelos, 2022
-}
module Main where

import Lexer
import Parser

main :: IO ()
main = do
  txt <- getLine
  print (parse $ alexScanTokens txt)
