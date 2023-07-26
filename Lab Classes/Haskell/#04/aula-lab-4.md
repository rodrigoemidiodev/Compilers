---
title: Compiladores &mdash; Folha laboratorial 4
author: Pedro Vasconcelos, DCC/FCUP
date: Outubro 2022
papersize: a4
lang: portuguese
...

# Análise sintática LR

## Exercício 1

(a) Mostre que a gramática de parêntesis simples

	$$ A \to \texttt{(} A \texttt{)}  ~~\mid~~ \texttt{a} $$

	é $LR(0)$ construindo o autómato e a tabela de *parsing* LR.

(b) Mostre que a gramática 

	$$ \begin{array}{l}
		T \to R \\
		T \to a T c 
		\end{array} \qquad 
		\begin{array}{l}
		R \to \varepsilon \\
		R \to bR
		\end{array} $$
		
	é $SLR(1)$ construindo o autómato e a tabela de *parsing* LR.
	Justifique que esta não é gramática $LR(0)$ mostrando 
	que existem conflitos na tabela de *parsing* $LR(0)$.


## Exercício 2 

Considere a seguinte extensão à gramática de parêntesis:

$$ \begin{array}{l}
E \to \texttt{(}L\texttt{)} ~~\mid~~~ \texttt{a} \\
L \to L\texttt{,} E  ~~\mid~~~ E
	\end{array}
$$

(a) Mostre uma derivação para a palavra `((a),a,(a,a))` 
(b) Construa o autómato de *parsing* $LR(0)$ da gramática
(c) Será que a gramática é $LR(0)$? Se não, mostre qual o conflito;
	se sim, construa a tabela de *parsing*.

\pagebreak

## Exercício 3

Considere a seguinte gramática simplificada de declarações de variáveis na
linguagem C; os terminais são `int`, `float`, vírgula (`,`), 
ponto-e-vírgula (`;`) e identificadores (*ident*):

$$
	\begin{array}{l}
	\textit{Decl} \to \textit{Type}~ \textit{Varlist}~\texttt{;} \\
	\textit{Type} \to \texttt{int} ~\mid~  \texttt{float} \\
	\textit{Varlist} \to \textit{Varlist} \texttt{,} \textit{ident} ~\mid~ \textit{ident} 
	\end{array}
$$ 

(a) Construa o autómato LR e a tabela de parsing $LR(0)$
(b) Simule os passos de execução do autómato para a entrada `int x,y,z;`


## Exercício 4

Considere a seguinte gramática (ambígua) para expressões aritméticas simples:

$$ \begin{array}{l}
	E \to E \texttt{+} E \\
	E \to E \texttt{-} E 
	\end{array} 
	\qquad
	\begin{array}{l}
	E \to E \texttt{*} E \\
	E \to E \texttt{/} E 
	\end{array} \qquad
	\begin{array}{l}
	E \to \texttt{(} E \texttt{)} \\
	E \to \textit{num}
	\end{array} 
	$$

(a) Implemente uma calculadora para expressões desta gramática usando
	geradores de analisadores léxicais e sintáticos.
	Use o *Alex*/*Happy* com a linguagem Haskell ou o *Flex*/*Bison* com a linguagem C.
	
	Resolva as ambiguidades usando diretivas de associatividade e precedência.
	Tenha o cuidado de verificar se a resolução 
	respeita as convenções algébricas usuais. Exemplos:
	
	-----------------  -------------------------------- -------------------------------------
	`1+2*3`            deve calcular $1+(2\times 3)$    (prioridade de `*` em relação a `+`)
	`1+2/3`            deve calcular $1+(2/3)$          (prioridade de `/` em relação a `+`)
	`1-2-3`            deve calcular $(1-2)-3$          (associatividade à esquerda)
	`1/2/3`            deve calcular $(1/2)/3$          (associatividade à esquerda)
	-----------------  -------------------------------- --------------------------------------
	
	O seu programa não necessita de construir a \emph{árvore sintática abstrata};
    pode efetuar os cálculos durante a análise sintática.
    O resultado deve ser calculado em vírgula flutuante de precisão simples.

<!--
## Exercício 5
-->

(b) Modifique a análise lexical, sintática e a função de avaliação
	para acrescentar algumas funções matemáticas  à calculadora:
    
	$$
	\begin{array}{l}
	E \to ~\cdots ~\mid~ F\texttt{(}E\texttt{)} \\
	F \to ~\texttt{sqrt} ~\mid~ \texttt{exp} ~\mid~~ \texttt{log}
	\end{array}
	$$

