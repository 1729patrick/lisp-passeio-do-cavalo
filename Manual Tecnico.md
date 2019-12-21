 # Inteligência Artificial - Jogo do Cavalo

## Manual Técnico
---
### Autores:
* Henoch Mendes Vitureira Nº170221014
* Patrick Battisti Forsthofer Nº190200007

## Docentes:
* Prof. Joaquim Filipe
* Eng. Filipe Mariano

# Índice
1. [Introdução](#Introdução)
   <br>
   1.1 [Objetivos](#Objetivos)
     <br>
   1.2 [Descrição Geral do Funcionamento](#Descrição)
2. [Utilização do Programa](#Utilização)
     <br>
   2.1 [Utilização do Programa](#Utilização)
      <br>
   2.2 [Pré-requisitos](#Pré-requisitos)
     <br>
   2.3 [Carregamento de Ficheiros do Programa](#Ficheiros)
     <br>
   2.4 [Inicar o Jogo](#Inicar)
     <br>
   2.5 [Estatísticas](#Estatísticas)
3. [Limitações](#Limitações)


# Introdução <a name="Introdução"></a>
Este documento tem como o objetivo documentar tecnicamente a implentação em Common Lisp do jogo do cavalo, uma variante do problema do cavalo, cuja finalidade é processar um dado tabuleiro pontuado de modo a atinguir uma pontuação desejada e apresentar a solução do problema em questão, com recurso a algoritmos de procura. Os algoritmos implementados foram o Deapth-First Search, o Breadth-First Search e o A*.

A primeira parte do documento dá a conhecer a estrutra de ficheiros do programa assim como o conteúdo dos mesmos. Posteriormente, são demonstrados os algoritmos implementados e os objetos que compôem o projeto.

Para finalizar, são apresentadas as limitações técnicas do programa.

# Ficheiros: <a name="Ficheiros"></a>
* <b>Project.lisp</b>:Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
  <br>
* <b>puzzle.lisp</b>: Código relacionado com o problema.
  <br>
* <b>search.lisp</b>: Código independente do problema. Contem a implementação dos algoritmos de Procura de Largura Primeiro (BFS), Procura do Profundidade Primeiro (DFS)e Procura do Melhor Primeiro (A*).

## Ficheiro puzzle.lisp
Este ficheiro contém função que auxiliaram acessos e cálculo de modo a promover a modularidade e seleçao de informação no projeto.

### Incialização dos Algoritmos
As funções abaixo foram as que serviram para a inicalização dos algoritmos. Nestes são pedidas todas as informações necessárias do utilizador para as funções poderem correr. Nomeadamente a posição inicial do cavalo e a estratégia de remoção de números "duplos". Existe uma função deste tipo para cada algoritmo implementado.
```lisp

(defun dfsearch (board)
"Initializes the DFS algorithm with a provided board"
  (let* (
         (start-pos (read-start-position))
         (start-points (car (successor-value (first start-pos) (second start-pos) board)))
         (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
         (strategy (read-strategy))
         (open-nodes (list (make-root-node board-with-horse start-points nil strategy)))
         (start-time (current-time))
         )
    (write-bfsdfs-statistics board (dfs open-nodes (list nil) (read-target-points) (read-depth) strategy) start-time (current-time) 'DFS)     
    )            
  )

(defun bfsearch (board)
"Initializes the BFS algorithm with a provided board"
  (let* (
         (start-pos (read-start-position))
         (start-points (car (successor-value (first start-pos) (second start-pos) board)))
         (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
         (strategy (read-strategy))
         (open-nodes (list (make-root-node board-with-horse start-points nil strategy)))
         (start-time (current-time))
         )

    (write-bfsdfs-statistics board (bfs open-nodes (list nil) (read-target-points) strategy) start-time (current-time) 'BFS)        
    )            
  )

(defun a*search (board heuristic)
"Initializes the A* algorithm with a provided board"
 
  (let* (
         (start-pos (read-start-position))
         (start-points (car (successor-value (first start-pos) (second start-pos) board)))
         (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
         (strategy (read-strategy))
         (open-nodes (list (make-root-node board-with-horse start-points nil strategy)))
         (start-time (current-time))
         )
  
    (write-bfsdfs-statistics board (a* open-nodes (list nil) (read-target-points) strategy heuristic) start-time (current-time) 'A*)
    )  
  )
```
### Construção de Nós
A estrutura de nós implementada foi a seguinte: (node)::= ((state)  (parent-node) (depth) (h) (f)). Nesta estrutura, "state" representa um estado, parent-node o nó antesessor, depth a profundidade do nó atual, "h" o valor heurístico e "f" o valor da soma entre g e h, isto é, o custo e o seu valor heurístico.
Os métodos abaixo serviram para construir nós, nós solução, que seriam utilizados na representação de estatísticas, e nós raiz.
```lisp

;;<node>::= (<state>  <parent-node> <depth> <h> <f>)
;;<state>::= (<board> <point-sum>)
(defun make-node (board points parent-node &optional (depth 0) (f nil))
"constructs a node with the structure <node>::= (<state>  <parent-node> <depth> <h> <f>)"
  (list (list board points) parent-node depth f)
  )
  
(defun make-solution-node (solution-path board open-list closed-list &optional depth target-points current-points)
"constructs a list that is used to write the statistics"
   (list solution-path board (length open-list) (length closed-list) depth target-points current-points)
  )

(defun make-root-node (board-placed-horse current-points parent &optional strategy)
"constructs root node"
  (make-node (remove-simmetric current-points board-placed-horse strategy) current-points nil 0 0)
  )
```
### Tabuleiros<a name="Tabuleiros"></a> 
Um tabuleiro representa-se por uma lista de listas, em que as listas contidas pela primeira representam as linhas do tabuleiro. Para obter os tabuleiros que o projeto requisitava que fossem resolvidos e documentados, foram criadas funções do seguinte tipo:
```lisp

(defun board-a ()
  "Returns board A, GOAL: 70"
  '(
    (02 20 44 nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil 03 30 nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil 22 nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    )
  )
```


### Seletores<a name="Utilização"></a> 
Foram implementados diversos seletores para a estrutura do nó, de modo a facilitar a seleção de dados do mesmo.

```lisp

;; parent-node
(defun parent-node (node)
"gets the parent of a node"
  (cond 
   ((null node) nil)
   (t (second node))
  )
)
;; test: (parent-node (test-node))
;; resultado: NIL
			
;; stade-node
(defun state-node (node)
"gets the depth of a node"
  (first node)
)

;; node-state-board
(defun node-state-board (node)
"gets the state of a node"
  (first (state-node node))
)

;; node-state-point-sum
(defun node-state-point-sum (node)
"gets the summed points of a node"
	(cond 
        ((null node) 0)
        (t (second (state-node node)))
	)
)

;; node-f
(defun node-f (node)
"gets the value of a node"
	(cond 
		((null node) nil)
		(t (fourth node))
	)
)
```
Além de outros métodos seletores, foi implementado um método para converter uma posição do tabuleiro numa posição de xadrez correspondente. esta serviu para mostrar a solução de um problema.

```lisp

(defun position-to-chess (horse-pos)
"returns a positition converted into a chess position"
	(list (string (code-char (+ 65 (second horse-pos)))) (+ 1 (first horse-pos)))
)
```

## Ficheiro search.lisp<a name="search"></a> 
Este ficheiro, tal como descrito anteriormente, contém a implementação dos algoritmos.

Abaixo encontra-se a implementação do algoritmo <b>Deapth-First Search</b>, isto é, procura por profundidade. Inicialmente o algoritmo atribui ao nó atual o primeiro nó da lista de abertos, que é fornecido no chamamento da função, e uma lista de nós fechados, que começa vazia. O critério de paragem do algoritmo é dado pela confirmação da verificação de se a lista de nós abertos está vazia ou se chegou ao número de pontos desejado. Caso não haja solução encontada num iteração, o algoritmo é executado recursivamente, adicionando os sucessores do nó atual ao início da lista de nós abertos e o nó atual à lista de nós fechados (expandidos).

```lisp
(defun dfs (open-list closed-list target-points max-depth strategy)
  (cond ((null open-list) (format t "No solution found"))
        (t 
         (let*  (
                 (node (car open-list))
                 (horse-pos (horsep node))
                 (current-board (horsep node))
                 (current-successors (successors (first horse-pos) (second horse-pos) node max-depth strategy 0 (append closed-list open-list)))
                 )

           (cond 
            ((<= target-points (node-state-point-sum node)) 
                  (format-output node "DFS")
                  (make-solution-node (reverse (get-solution-path node)) (node-state-board node) open-list closed-list max-depth target-points (node-state-point-sum node))
                  )
            (t                 
             (dfs 
              (append current-successors (cdr open-list))
              (append (cdr closed-list) (list node))
              target-points
              max-depth
              strategy
              )
             ))))))
```
O algoritmo <b>Breadth-First Search</b> foi implementado de forma idêntica à do Deapth-First Search, tirando na forma de como concatenar os os sucessores, que são colocados no final da lista de abertos.


## Estatísticas<a name="Estatísticas"></a>
Após a execução bem sucedida de um algoritmo, o programa irá escrever no ficheiro "statistics.dat" a análise estatística da execução do algoritmo para resolver o tabuleiro escolhido. As informações oferecidas passam por ser algoritmo utilizado, tempo de execução, número de nós gerados, números de nós expandidos, valor de penetrância, factor de ramificação média, profundidade máxima (no caso de Depth-First), tamanho da solução, pontos desejados, pontos alcançados, sequência de jogadas, e os tabuleiros inicial e final. Abaixo encontra-se um exemplo deste output.
```dat
 ----: Algorithm: DFS 
 ----:  Starting time: 17:55:31
 ----:  Ending Time: 17:55:46
 ----:  Number of Generated Nodes: 32
 ----:  Number of Expanded Nodes: 16
 ----:  Penetrance Level: 0.1875
 ----:  Median Branching Factor: 1.4831691
 ----:  Maximum Depth: 6
 ----:  Solution Length: 6
 ----:  Goal Points: 240
 ----:  Current Points: 245
 ----:  Solution Sequence: ((D 1) (C 3) (B 1) (A 3) (B 5) (A 7))
 ----:  Starting Board:
   (94 25 54 89 21 8 36 14 41 96)
   (78 47 56 23 5 49 13 12 26 60)
   (0 27 17 83 34 93 74 52 45 80)
   (69 9 77 95 55 39 91 73 57 30)
   (24 15 22 86 1 11 68 79 76 72)
   (81 48 32 2 64 16 50 37 29 71)
   (99 51 6 18 53 28 7 63 10 88)
   (59 42 46 85 90 75 87 43 20 31)
   (3 61 58 44 65 82 19 4 35 62)
   (33 70 84 40 66 38 92 67 98 97)
 ----:  Final Board:
   (94 NIL 54 NIL 21 8 36 14 41 96)
   (78 47 56 23 5 49 13 12 26 60)
   (NIL 27 NIL 83 34 93 74 NIL 45 80)
   (69 9 77 95 55 39 91 73 57 30)
   (24 NIL 22 86 1 NIL 68 79 76 72)
   (81 48 32 2 64 16 50 37 29 NIL)
   (T NIL 6 18 53 28 7 63 10 88)
   (59 42 46 85 90 75 87 43 20 31)
   (3 61 58 44 65 82 19 4 35 62)
   (33 70 84 40 66 38 92 67 NIL 97)
```

# Limitações<a name="Limitações"></a>

Em termos de limitações, o programa não consegue voltar a atrás nas ações do utilizador, nem começar o programa de novo sem chamar a função "start-game" de novo. Os inputs do utilizador não têm validações para todo o tipo de erros.