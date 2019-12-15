;;; Passeio do cavalo
;;; variaveis de teste e operadores
(defun test-node (board)
"Define um no teste do problema do passeio do cavalo em que valor=77, profundidade=0, pai=NIL, tempo-de-criaçao=current-time"
 (list '(0 2) board 0 nil (current-time)))
 
 ;;; Construtor
(defun make-node (position board &optional (depth 0) (pai nil))
  (list position board depth (current-time) pai)
)
 
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

										
;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
(defun solution-nodep (target-points)
  ;;;todo
  (list nil)
)
;; teste: 
;; resultado: NIL