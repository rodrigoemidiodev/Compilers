{
-- Analisador sintático para a calculadora simples
module Parser where
import Lexer  
}

%name parse 
%tokentype { Token }
%error { parseError }

%token

num { TOK_NUM $$ }
'+' { TOK_PLUS }
'-' { TOK_MINUS }
'*' { TOK_MULT }
'/' { TOK_DIV }
'(' { TOK_LPAREN }
')' { TOK_RPAREN }
'sqrt' { TOK_SQRT }
'exp' { TOK_EXP }
'log' { TOK_LOG }

%left '+' '-'
%left '*' '/'

%%

Exp : num	       { $1 }
    | Exp '+' Exp  { $1 + $3 }
    | Exp '-' Exp  { $1 - $3 }
    | Exp '*' Exp  { $1 * $3 }
    | Exp '/' Exp  { $1 / $3 }
    | '(' Exp ')'  { $2 }
    | 'sqrt' '(' Exp ')' { sqrt $3 }
    | 'exp' '(' Exp ')' { exp $3 }
    | 'log' '(' Exp ')' { log $3 }

-- completar as regras da gramática
-- tenha em atenção de implementar as regras de precedência
-- e associatividade entre operadores!

{
parseError :: [Token] -> a
parseError toks = error "parse error"  
}

