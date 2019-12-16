;;; Passeio do cavalo
;;; variaveis de test e operadores

;;<node>::= (<state>  <parent-node> <depth> <h> <f>)
;;<state>::= (<board> <point-sum>)
(defun test-node (board)
;Define um no test do problema do passeio do cavalo, em que, estado=(tabuleiro, soma-pontos), depth=0, parent-node=NIL, 
;and criation time=current-time"
 (list (list board (cell 0 2 board)) nil 0 nil nil))
 

 ;;; Construtor
(defun make-node (board points parent-node &optional (depth 0) (heuristic nil) (f nil))
     (list 
		(list board points) 
		parent-node
		depth  
		heuristic	
		f
    ))
 
 ;;; Boards
(defun test-board ()
"A test board without any play made"
  '(
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
    )
)

(defun played-board ()
"A board just like the previous but with a play made in the position 94"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 NIL 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

;;access to line and cell
(defun line (num board)
"returns the line of the board with index=num"
	(nth num board)
)
;;test: (line 0 (test-board))
;; result: (94 25 54 89 21 8 36 14 41 96)

(defun cell (line col board)
"returns the cell of the board with line-index=line and column-index= col"
	(nth col (line line board))
)
;;test: (cell 0 1 (test-board))
;; result: 25


;; no-profundidade
(defun depth-node (node)
	(cond ((null node) 0)
	  (t (third node))
	  )
)
;; test: (no-profundidade (no-test))
;; resultado: 0

;; parent-node
(defun parent-node (node)
  (cond 
   ((null node) nil)
   (t (second node))
  )
)
;; test: (parent-node (test-node))
;; resultado: NIL
			
;; stade-node
(defun state-node (node)
  (first node)
)

;; node-state-board
(defun node-state-board (node)
  (first (state-node node))
)

;; node-state-point-sum
(defun node-state-point-sum (node)
	(cond 
		((null node) 0)
		(t (second (state-node node)))
	)
)

;; node-heuristic
(defun node-heuristic (node)
	(cond 
		((null node) nil)
		(t (fourth node))
	)
)

;; node-f
(defun node-f (node)
	(cond 
		((null node) nil)
		(t (fifth node))
	)
)

;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
(defun solution-nodep (target-points node)
  (cond
    ((>= (node-state-point-sum node) target-points) T)
     (t nil)
  )
)
;; test: 
;; resultado: NIL

(defun position-to-chess (line col)
	(list (string (code-char (+ 65 col))) (+ 1 line))
)
;;test: (position-to-chess 0 0))
;;result: ("A" 1)