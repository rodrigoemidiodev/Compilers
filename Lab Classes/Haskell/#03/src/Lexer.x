--
-- Um analisador lexical para expressões aritméticas simples
--
{
module Lexer where
}

%wrapper "basic"

$white =  [\ \t\n\r]
$digit =  [0-9]

tokens :-

$white+  ; -- ignorar carateres brancos

"+"       { \_ -> PLUS }
"-"       { \_ -> MINUS }
"*"       { \_ -> MULT }
"/"       { \_ -> DIV }
"("       { \_ -> LPAREN }
")"       { \_ -> RPAREN }

$digit+   { \_ -> NUM }

-- NOTA:
-- como vamos apenas reconhecer expressões bem-formadas
-- não necessitamos de associar um valor ao *token* NUM

{

data Token
  = PLUS
  | MINUS
  | MULT
  | DIV
  | LPAREN
  | RPAREN
  | NUM
  deriving (Eq, Show)

}  
