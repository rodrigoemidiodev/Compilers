
# Compiladores &mdash; Aula laboratorial 3 

## Exercício 3: *parser* para expressões aritméticas em Haskell

Implemente um *parser* por descida recursiva para expressões artméticas.

Para mais detalhes ver a [folha de exercícios](aula-lab-3.pdf).

Comece por estudar o analisador lexical escrito usando *Alex* ou *Flex* 
Pode usar `cabal build` e `cabal run` para compilar e executar alguns testes
(ficheiros  `tests/input*.hs`).

Notas:

1. o comando `cabal` executa o Alex para gerar o `.hs` a partir
   do  `.x`;
   não é necessário executar manualmente
2. não deve acresentar o `Lexer.hs` ao repositório Git (uma
   vez que este será gerado automaticamente).

