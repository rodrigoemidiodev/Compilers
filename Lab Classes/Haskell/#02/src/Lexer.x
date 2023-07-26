{
module Main where
}

%wrapper "basic"
$alpha = [_a-zA-Z]
$digit = [0-9]

tokens :-

$white+	; -- Ignorar caracteres brancos

-- Palavras reservadas
if { \_ -> IF }
while { \_ -> WHILE }
for { \_ -> FOR }
int { \_ -> INT }
float { \_ -> FLOAT }

-- Sinais de pontuação
"("	{ \_ -> LPAREN }
")"	{ \_ -> RPAREN }
"{" { \_ -> LBRACE }
"}" { \_ -> RBRACE }
"," { \_ -> COMMA }
";" { \_ -> SEMICOLON }

-- Comentários
"//".* ; -- Comentário de uma linha
"/*"(\n|.)+ ; -- Comentário de várias linhas

-- Identificadores, inteiros e reais
$alpha($alpha|$digit)* { \s -> ID s }
$digit+	{ \s -> NUM (read s) }
($digit+"."$digit*) | ($digit*"."$digit+) { \s -> REAL (read s) }

{
data Token = ID String 
           | NUM Int 
           | REAL Double
           | LPAREN 
           | RPAREN 
           | LBRACE 
           | RBRACE 
           | COMMA
           | SEMICOLON
           | IF
           | WHILE
           | FOR
           | INT
           | FLOAT
           deriving (Eq, Show)

main :: IO ()
main = do
  txt <- getContents
  print(alexScanTokens txt)
}
