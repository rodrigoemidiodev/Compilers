---
title: Compiladores &mdash; Folha laboratorial 0
author: Pedro Vasconcelos, DCC/FCUP
date: Outubro 2021
papersize: a4
lang: pt
...


# Exercício 1: Crie uma conta de Github

Se ainda não tem uma conta de Github, comece por ir ao sítio
[https://github.com](https:://github.com) e crie uma nova conta ("Sign up").
Se já tiver uma conta, não precisa de criar uma nova, pode usar a que
já tem.

# Exercício 2: Aceite o trabalho 

Usando o "link" no Moodle, aceite trabalho laboratorial 0
(`aula-lab-0`). Terá de associar o seu utilizador de Github ao seu
nome e número mecanográfico na UP. Ficará com um novo repositório
`aula-lab-0`.  Experimente navegar pelos vários ficheiro do
repositório usando a interface web do Github.

Verá que tem dois diretórios com código fonte de dois programas em
linguagem C e Haskell que imprimem mensagens de boas vindas.

# Exercício 3: Clonar o repositório 

Execute uma ação de "clone" para obter uma cópia local do repositório 
no seu computador (substituindo `utilizador` pelo seu utilizador de Github):

~~~
git clone https://github.com/utilizador/aula-lab-0
~~~

Experimente compilar os programas de exemplo (necessita das
ferramentas de desenvolvimento de C e Haskell).

# Exercício 4: Modificar os programas

Experimente modificar a mensagem impressa pelos programas (por
exemplo, com o seu nome).

Adicione as alterações e efetue um novo *commit*:

~~~
git add src/Hello.hs src/hello.c
git commit -m "personalização das mensagens"
~~~

Em seguida efetue um *push* das alterações para o Github:

~~~
git push
~~~


# Exercício 5: Extras

Se ainda não tiver, crie um par de chaves pública/privada SSH usando o
comando `ssh-keygen` e adicione a chave pública ao seu perfil
(Settings > SSH and GPG keys).  Depois pode apagar a cópia local do
repositorio e clonar uma nova desta vez usando SSH em vez de HTTPS.
Desta forma não vai ter de digitar a palavra-passe de cada vez que
quiser fazer *push* ou *pull*.
