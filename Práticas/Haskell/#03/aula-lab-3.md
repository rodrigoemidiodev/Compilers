---
title: Compiladores &mdash; Folha laboratorial 3
author: Pedro Vasconcelos, DCC/FCUP
date: Outubro 2022
papersize: a4
...

# Gramáticas independentes de contexto

## Exercício 1

(a) Considere a gramática apresentada na aula teórica
	com símbolos terminais $\{a,b\}$, não-terminais $\{S, B\}$,  símbolo
	inicial $S$ e as produções seguintes.

	$$   \begin{array}{l}
      S  \to aSB  \\
      S  \to \varepsilon \\
      S  \to B    
	  \end{array} \qquad
	  \begin{array}{l}
      B  \to Bb   \\
      B  \to b 
	\end{array} $$
  
	Descreva a linguagem reconhecida por esta gramática numa frase.
	(Sugestões: Onde podem ocorrer $a$s e $b$s? Qual
	relação entre as suas contagens?)

(b) Escreva uma gramática para a linguagem de palavras com o mesmo número
    de $a$s e $b$s (em qualquer ordem)

(c)	Escreva uma gramática para a linguagem de parêntesis casados.
	Alguns exemplos:
	$$ \begin{array}{c}
	\text{Palavras aceites} \\ \hline
	\varepsilon \quad (\text{palavra vazia})\\
	() \\
	(()) \\
	()() \\
	(()(()))
	\end{array}
	\qquad
	\begin{array}{c}
	\text{Palavras não aceites} \\ \hline
	( \\ ) \\
	)( \\  (() \\
	()())
	\end{array} $$
	
(d) Escreva uma gramática para a linguagem da expressão regular
	`((ab*a)|(ba*b))`

(e) Escreva uma gramática para a linguagem da expressão regular
	`((0|1)+"."(0|1)*)|((0|1)*"."(0|1)+)`

## Exercício 2

Considere uma gramática de programas sequências apresentada na aula teórica:
$$\begin{array}{l}
    S  \to S \;\texttt{;}\; S \\
    S  \to \text{ident}\; \texttt{=}\; E \\
    S  \to \text{ident}\; \texttt{++}
  \end{array}
  \qquad  \qquad
  \begin{array}{l}
    E  \to \text{ident} \\
    E  \to \text{num} \\
    E  \to E \,\texttt{+}\, E \\
    % E  \to E \,\texttt{-}\, E \\
    % E  \to E \,\texttt{*}\, E \\
    % E  \to E \,\texttt{/}\, E \\
    % E  \to ( E ) \\    
    % E  \to \texttt{(}S \;\texttt{,}\; E \texttt{)}
  \end{array}
  $$

(a) Mostre que a gramática acima é ambígua, i.e., encontre uma palavra com duas
	derivações correspondentes a árvores distintas. Consegue encontrar
	mais do que um exemplo?
(b)	Re-escreva a gramática de forma a eliminar a ambiguidade.	

## Exercício 3

Considere a gramática para expressões aritméticas fatorizada
de forma a eliminar ambiguidade e recursão à esquerda:
  $$ \begin{array}{l}
       E \to T~  E' \\[1ex]
       E' \to +~ T~ E' \\
       E' \to -~ T~ E' \\
       E' \to \varepsilon
     \end{array}
     \qquad
     \begin{array}{l}
       T \to F~ T' \\[1ex]
       T' \to *~ F~ T' \\
       T' \to /~ F~ T' \\
       T' \to \varepsilon
     \end{array}
     \qquad
     \begin{array}{l}
       F \to \text{num}\\
       F \to (~E~) 
     \end{array}
     $$

Partindo do esqueleto de código no repositório *Git*, implemente um
*parser* para esta gramática usando descida recursiva.  O *parser*
deve aceitar expressões bem formadas como `123*(45+6)` ou
`(1+2*3)/(4+5)` e rejeitar expressões incorretas como `)12+53(` ou
`(1++2*3)`.


## Exercício 4

Considerando de novo a gramática do Exercício 3:

(a) Determine o valor de NULLABLE e dos conjuntos FIRST e FOLLOW
	para os não-terminais desta gramática. (Sugestão: comece por
	determinar as repostas por inspeção das produções
	e só depois use as equações apresentadas na aulas 
	para verificar se a sua intuição está correta.)

(b) Mostre que a gramática acima é $LL(1)$ calculado a tabela de *parsing*
	preditivo.


