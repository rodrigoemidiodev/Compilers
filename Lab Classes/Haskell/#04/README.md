# Compiladores &mdash; Folha laboratorial 4

## Exercício 4: uma calculadora para expressões aritméticas simples

Implemente em Haskell uma calculadora para operações aritméticas
simples usando geradores de analisadores lexicais e sintáticos.

Para detalhes deve consultar a [folha de exercícios](aula-lab-4.pdf).


## Recomendações 

* Deve editar o esqueleto de código fonte no diretório `src`
* Não adicione ao repositório Git ficheiros gerados automaticamente;
  exemplo: deve acrescentar os ficheiros
  `Lexer.x` e `Parser.y` mas **não adicione** 
  `Lexer.hs` e `Parser.hs` (que são gerados a partir os anteriores)
* Para construir e executar o programa deve usar a ferramenta `cabal`:
  
  ~~~
  $ cabal v1-build
  $ echo "1+2+3" | cabal v1-run
  ...
  6.00000
  ~~~
* Experimente correr os exemplos (`tests/input*.txt`) e compare com os
  resultados esperados (`tests/output*.txt`)


  
## Documentação 

Alex

:   [https://www.haskell.org/alex/doc/html/index.html](https://www.haskell.org/alex/doc/html/index.html)

Happy

:   [https://www.haskell.org/happy/doc/html/](https://www.haskell.org/happy/doc/html/)

