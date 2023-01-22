module Main where
import Lexer

main = do
    txt <- getContents
    print (alexScanTokens txt)