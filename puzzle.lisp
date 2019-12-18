(defun dfsearch (board)
  (let* (
         (start-pos (read-start-position))
         (start-points (car (successor-value (first start-pos) (second start-pos) board)))
         (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
         (strategy (read-strategy))
         (open-nodes (list (make-successor-node-dfs board-with-horse start-points nil strategy)))
         )

    (dfs open-nodes (list nil) (read-target-points) (read-depth) strategy)
    )            
  )

(defun bfsearch (board)
  (let* (
         (start-pos (read-start-position))
         (start-points (car (successor-value (first start-pos) (second start-pos) board)))
         (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
         (strategy (read-strategy))
         (open-nodes (list (make-successor-node-dfs board-with-horse start-points nil strategy)))
         )

    (bfs open-nodes (list nil) (read-target-points) strategy)
    )            
  )



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
(defun board-order ()
  '(
    (0 1 2 3 4 5 6 7 8 9)
    (10 11 12 13 14 15 16 17 18 19)
    (20 21 22 23 24 25 26 27 28 29)
    (30 31 32 33 34 35 36 37 38 39)
    (40 41 42 43 44 45 46 47 48 49)
    (50 51 52 53 54 55 56 57 58 59)
    (60 61 62 63 64 65 66 67 68 69)
    (70 71 72 73 74 75 76 77 78 79)
    (80 81 82 83 84 85 86 87 88 89)
    (90 91 92 93 94 95 96 97 98 99)
    )
)

(defun board-a ()
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


(defun board-b ()
  '(
    (02 nil 04 nil 06 nil 08 nil 10 nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil 03 nil 05 nil 07 nil 09 nil 11)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    )
  )


(defun board-c ()
  '(
    (01 12 03 23 nil 88 nil nil nil nil)
    (21 45 43 nil nil nil nil nil nil nil)
    (nil 56 nil 78 nil nil nil nil nil nil)
    (89 nil 99 54 nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    )
  )


(defun board-d ()
  '(
    (98 97 96 95 94 93 92 91 90 89)
    (01 02 03 04 05 55 06 07 08 09)
    (nil 66 nil nil nil nil nil nil nil 11)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil 22 nil nil nil nil nil 33 nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil 88 nil nil nil 44 nil nil)
    (nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil 77 nil nil nil nil nil)
    (nil nil nil nil nil nil 99 nil nil nil)
    )
  )


(defun board-e ()
  '(
    (nil 05 nil nil nil 15 nil nil nil 25)
    (nil nil nil 06 nil nil nil 16 nil nil)
    (nil 04 nil nil nil 14 nil nil nil 24)
    (nil nil nil 07 nil nil nil 17 nil nil)
    (nil 03 nil nil nil 13 nil nil nil 23)
    (nil nil nil 08 nil nil nil 18 nil nil)
    (nil 02 nil nil nil 12 nil nil nil 22)
    (nil nil nil 09 nil nil nil 19 nil nil)
    (nil 01 nil nil nil 11 nil nil nil 21)
    (nil nil nil 10 nil nil nil 20 nil nil)
    )
  )


(defun board-f ()
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

(defun position-to-chess (horse-pos)
	(list (string (code-char (+ 65 (second horse-pos)))) (+ 1 (first horse-pos)))
)
;;test: (position-to-chess 0 0))
;;result: ("A" 1)



(defun number-generated-nodes (open closed)
  "Total number of generated nodes: open+closed"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

(defun solution-length (list-solution)
  "Devolve o comprimento de uma  solucao"
  (length list-solution)
  )

;;BRANCHING FACTOR
(defun polinomial-sum (B L t-value)
 "B + B^2 + ... + B^L=T"
  (cond
   ((= 1 L) (- B t-value))
   (T (+ (expt B L) (polinomial-sum B (- L 1) t-value)))
  )
)

(defun branching-factor (solution-path-list open closed &optional (L-value (solution-length solution-path-list)) (T-value (number-generated-nodes open closed)) (max-error 0.1) (bmin 1) (bmax 10e11))
  "Devolve o factor de ramificacao, executando o metodo da bisseccao"
  (let ((b-average (/ (+ bmin bmax) 2)))
    (cond 
     ((< (- bmax bmin) max-error) (/ (+ bmax bmin) 2))
     ((< (polinomial-sum b-average L-value T-value) 0) (branching-factor solution-path-list L-value T-value max-error b-average bmax))
     (t (branching-factor solution-path-list L-value T-value max-error bmin b-average))
     )
    )
  )


(defun get-solution-path (node)
  (cond ((null node) nil)
        (t (cons  (position-to-chess (horsep node)) 
                  (get-solution-path (parent-node node)))  
           )
        )
  )


;;Recursively print the elements of a list
(defun print-list (elements)
    (cond
        ((null elements) '()) ;; Base case: There are no elements that have yet to be printed. Don't do anything and return a null list.
        (t
            ;; Recursive case
            ;; Print the next element.
            (write-line (write-to-string (car elements)))
            (print-list (cdr elements))
            ;; Recurse on the rest of the list.
        )
    )
)

(defun format-output (node algorithm)
  (terpri)
  (format t "*********************** SOLUTION ~A ***********************************" algorithm)
  (terpri)
  
  (format t "POINTS: ~A" (node-state-point-sum node))
  (terpri)
  (terpri)

  (format t "DEPTH: ~A" (depth-node node))
  (terpri)
  (terpri)

  (format t "SEQUENCE: ")
  (terpri)
  (print-list (reverse (get-solution-path node)))
  (terpri)
  (terpri)  

  (format t "BOARD:")
  (terpri)
  (print-list (node-state-board node))
  (format t "********************************************************************")
  )
