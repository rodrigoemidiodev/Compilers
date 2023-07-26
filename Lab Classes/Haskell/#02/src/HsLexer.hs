{-
  Um analisador lexical simples em Haskell
  Pedro Vasconcelos, 2022
-}
module Main where

import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

-- Tipo algébrico para tokens;
-- alguns dos casos ainda não estão implementados
data Token = ID String       -- e.g. xy123
           | NUM Int         -- e.g. 123
           | REAL Float      -- e.g. 123.45
           | LPAREN          -- (
           | RPAREN          -- )
           | LBRACE          -- {
           | RBRACE          -- }
           | COMMA           -- ,
           | SEMICOLON       -- ;
           | IF              -- if
           | WHILE           -- while
           | FOR             -- for
           | INT             -- int
           | FLOAT           -- float
           deriving (Eq,Show)

--
-- transformar lista de carateres numa lista de tokens
--
lexer :: [Char] -> [Token]
lexer [] = []
lexer (',':xs) = COMMA : lexer xs
lexer ('(':xs) = LPAREN : lexer xs
lexer (')':xs) = RPAREN  : lexer xs
lexer ('{':xs) = LBRACE : lexer xs
lexer ('}':xs) = RBRACE : lexer xs
lexer (';':xs) = SEMICOLON : lexer xs
lexer ('/':'/':xs) = let xs' = dropWhile (/= '\n') xs in lexer xs'
lexer ('/':'*':xs) = lexer xs'
         where xs' = consume xs
lexer (x:xs)
  | isSpace x = lexer xs        -- consumir carateres brancos
lexer (x:xs)
  | isDigit x && y == '.' = REAL (read (x:xs' ++ [y] ++ ys)) : lexer ys'  
  | isDigit x = NUM (read (x:xs')) : lexer xs''
  where xs' = takeWhile isDigit xs
        xs''= dropWhile isDigit xs
        ys = takeWhile isDigit (tail xs'')
        ys' = dropWhile isDigit (tail xs'')
        y = head xs''
lexer (x:xs)        
  | isAlphaUnder x = lexerText (x:xs') : lexer xs''
  where xs' = takeWhile isAlphaNumUnder xs
        xs''= dropWhile isAlphaNumUnder xs
lexer (x:_)       -- outros carateres: erro
  = error ("invalid character: " ++ show x)


-- função auxiliar para distinguir palavras reservadas de identificadores
lexerText :: String -> Token
lexerText "if" = IF
lexerText "while" = WHILE
lexerText "for" = FOR
lexerText "int" = INT
lexerText "float" = FLOAT
lexerText xs = ID xs

-- consumir o comentário
consume :: [Char] -> [Char]
consume ('*': '/': xs) = xs
consume (_:xs) = consume xs

-- verificar se é underscore ou letra
isAlphaUnder :: Char -> Bool
isAlphaUnder c = c == '_' || isAlpha c 

-- verificar se é underscore, letra ou dígito
isAlphaNumUnder :: Char -> Bool
isAlphaNumUnder c = c == '_' || isAlpha c || isDigit c

-- ler toda a entrada e imprimir a lista de tokens
main :: IO ()
main = do
  txt <- getContents
  print (lexer txt)

  
