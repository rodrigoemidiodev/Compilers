{-# LANGUAGE ScopedTypeVariables #-}
{-
   Parser para expressões aritmética por descida recursiva

   Programa principal:
   - lê uma linha da entrada padrão
   - executa o lexer e parser
   - verifica se o parsing terminou com erro ou sucesso
     e escreve uma mensagem apropriadada

   Pedro Vasconcelos
-}
module Main where

import Parser
import Lexer
import Control.Exception

main :: IO ()
main = do
  txt <- getLine
  check <- validExpr txt
  if check then
    putStrLn "Syntax OK"
    else
    putStrLn "Syntax error"

validExpr :: String -> IO Bool
validExpr txt
  = catch
       (return $! (parseE (alexScanTokens txt) == []))
       (\(e::SomeException) -> return False)


