;; inicia o algoritmo de busca dfs

;; (dfsearch  55 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 48 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; (55 34 13 32 11 3 22 41 62 81 73 52 33 12 24 45 64 83 71 92 84 63 44 65 86 67 88 69 50 58 39 20 28 9 30 49 70 89) 

(defun bfsearch (start graph)
  (bfs (list start) '()  graph)
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


  ;;Recursively print the elements of a list
(defun print-list-chess (elements)
    (cond
        ((null elements) '()) ;; Base case: There are no elements that have yet to be printed. Don't do anything and return a null list.
        (t
            ;; Recursive case
            ;; Print the next element.
            (progn
            (write-line (write-to-string (car elements)))
            (write-line (write-to-string (second elements)))
             )
            ;; Recurse on the rest of the list.
        )
    )
)



;;start dfs
(defun dfsearch (board)
      (let* (
           (start-pos (read-start-position))
           (start-points (car(successor-value (first start-pos) (second start-pos) board)))
           (board-with-horse (replace-value (first start-pos) (second start-pos) board T))
            (open-nodes (list 
               (make-successor-node-dfs board-with-horse start-points nil)
               ))
           )
          (dfs open-nodes (list nil) (read-target-points))
      )            
)

(defun make-successor-node-dfs (board-placed-horse current-points parent)
           
         (cond 
          ((null parent)
            (make-node 
                board-placed-horse
                (+ (node-state-point-sum parent) current-points)
                parent
              (depth-node parent)
              ))
         (t (make-node 
                    board-placed-horse
                    (+ (node-state-point-sum parent) current-points)
                    parent
                    (1+(depth-node parent))
                  ))
                    
         )
  )

(defun get-solution-path (node)

  
  (cond ((null node) nil)
   (t (cons  (position-to-chess (horsep node)) 
                 (get-solution-path (parent-node node)))
   ;;(get-solution-path (parent-node node))
  
  )
))

(defun dfs (open-list closed-list target-points)
  (cond ((null open-list) nil)
        (t 
         (let*  (
                  (node (car open-list))
                  (horse-pos (horsep node))
                  (current-board (horsep node))
                  (current-successors (successors (first horse-pos) (second horse-pos) node))
						   )
           (cond 
            ((or (null current-successors) (<= target-points (node-state-point-sum node)))
             (terpri)
             ;; SEQUENCIA DE JOGADAS
             (format t "DFS ENDED, POINTS GOTTEN:")
             (terpri)
             (write-line (write-to-string (node-state-point-sum node)))
             (terpri)
             (format t "SOLUTION SEQUENCE:")
             (terpri)
            
             
             (reverse (print-list (get-solution-path node)))
             (terpri)
             
             (format t "FINAL BOARD:")
             (terpri)
             (print-list (node-state-board node))
             (terpri)
             )
			 ;;((>(calculate-current-depth path) max-depth) (dfs (cdr open-list) graph max-depth))
            (t 
                    (terpri)
                ;; SEQUENCIA DE JOGADAS
                (format t "iteracao: " )
                (terpri)
                
                (print-list-chess (position-to-chess (horsep node)))
                
                (terpri)
                  (dfs 
                    (append current-successors (cdr open-list))

                    (append node (cdr closed-list))
                    target-points
                  )
          
                )
            ))
         )
        )
  )


(defun horsep (node)
  (successor-position T (node-state-board node))
)

  
  
  ;;;test
  (defun nbDigits (digit)
     (cond 
          ((< digit 10) 1)
          (t (1+ (nbDigits (truncate digit 10))))
      )
     )
  
  
(defun bfs (open closed graph)
  (cond ((null open) nil)
        (t 
         (let* (
                (node (car open))
                (successors (successors-path-bfs node graph))
                )

          (terpri)
          (format t "NODE: ~A" node)
          (terpri)
          (FORMAT T "SUCCESSORS: ~A" successors)
          (terpri)
          (format t "CLOSED: ~A" closed)
          (terpri)
          (format t "OPEN: ~A" open)
          (terpri)
						 
           (cond 
            ((null successors)
             (terpri)
             (format t "~A" closed)
             (terpri)
             )
            ((null (member node closed)) (bfs 
                (append (cdr open) successors)
                (append closed (list node))
                graph
                ))
            (t (bfs (cdr open) closed graph))
            ))
         )
        )
  )
	
;; busca os sucessores de um nó e retornao caminho até os sucessores

;; (successors-path  ‘(89 70 49 30 9 28 20 39 58 50 69 88 67 86 65 44 63 84 92 71 83 64 45 24 12 33 52 73 81 62 41 22 3 11 32 13 34 55)  89 ‘(
;; (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;; (NIL NIL NIL NIL 15 16 NIL NIL 19 NIL)
;; (NIL NIL NIL NIL NIL NIL 27 NIL NIL NIL) 
;; (NIL NIL NIL NIL 35 NIL NIL NIL NIL 40) 
;; (NIL NIL NIL NIL NIL NIL 47 NIL NIL NIL)
;; (51 NIL 53 NIL NIL NIL 57 NIL 59 60) 
;; (61 NIL NIL NIL NIL 66 NIL NIL NIL NIL)
;; (NIL 72 NIL 74 75 NIL 77 78 79 80) 
;; (NIL NIL NIL NIL NIL NIL 87 NIL 89 90) 
;; (91 NIL NIL NIL 95 NIL 97 98 99 100))
;; )

;; ((77 89 70 49 30 9 28 20 39 58 50 69 88 67 86 65 44 63 84 92 71 83 64 45 24 12 33 52 73 81 62 41 22 3 11 32 13 34 55)
;; (97 89 70 49 30 9 28 20 39 58 50 69 88 67 86 65 44 63 84 92 71 83 64 45 24 12 33 52 73 81 62 41 22 3 11 32 13 34 55))
(defun successors-path (path node graph)
  (let* (
         (line-column (successor-position node graph))
         )

    (cond ((or (null (first line-column)) (null (second line-column))) (list path))
          (t (mapcar #'(lambda (x)
                         (cons x path))
                     (successors (first line-column) (second line-column) graph))
             ))
    )
  )


(defun successors-path-bfs (node graph)
  (let* (
         (line-column (successor-position node graph))
         )

    (cond ((or (null (first line-column)) (null (second line-column))) '())
          (t (successors (first line-column) (second line-column) graph)))
     
    )
  )


;; busca o index da linha e o index da coluna de um nó

;; (successor-position 90 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 48 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; (8 9)
(defun successor-position (value graph)
  (let* (
         (line (successor-line value graph))
         )

    (list (position line graph :test #'equal) (position value line :test #'equal))
    )
  )


;; busca a linha onde está o nó

;; (successor-line 90 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 48 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; (81 82 83 84 85 86 87 88 89 90)

(defun successor-line (value graph)
  (apply #'append
         (mapcar
          (lambda (lin) 
            (cond
             ((position value lin :test #'equal) lin)
             (T nil)
             )
            )
          graph
          )
         )        
  )

;; busca o valor de um nó no grafo pelo index da linha e index da coluna ou retorna nil para indexes inválidos

;;  (successor-avaliable 9 16 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 48 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))
              
;; NIL
(defun successor-avaliable (line-index column-index node)
  (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   ((null (car(successor-value line-index column-index (node-state-board node)))) nil)
    (t 
       (let* (
              (horse-pos (horsep node))
              (node-board (node-state-board node))
              (points-to-sum (car(successor-value line-index column-index node-board)))
              (board-no-horse (replace-value (first horse-pos) (second horse-pos) (remove-node points-to-sum node-board)))
              ;;(board-no-simetric (remove-simetric points-to-sum board-no-horse))
              (board-to-be (replace-value line-index column-index board-no-horse T))
              
              
             )

             (cond ((null points-to-sum) nil)
              (t  
                ;;trying return list
                (list 
                    (make-successor-node-dfs board-to-be points-to-sum node))
              )        
              )
            
        )
    
    )
   )
  )


;; busca o valor de um nó no grafo pelo index da linha e index da coluna

;; (successor-value 9 8 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 48 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; (99)
(defun successor-value (line-index column-index graph)
  (let* (
         (line (nth line-index graph))
         (value (nth column-index line))
         )
    
    (cond 
     ((null value) nil)
     (t (list value))
     )
    )
  )
 

;; busca todos os sucessores válidos de um nó

;; (successors 5 5 ‘((1 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; (35 37 75 77 44 64 68)
(defun successors (line-index column-index node)
  (append
   (successor-avaliable (- line-index 2) (- column-index 1) node)
   (successor-avaliable (- line-index 2) (+ column-index 1) node)
   (successor-avaliable (+ line-index 2) (- column-index 1) node)
   (successor-avaliable (+ line-index 2) (+ column-index 1) node)
   (successor-avaliable (- line-index 1) (- column-index 2) node)
   (successor-avaliable (- line-index 1) (+ column-index 2) node)
   (successor-avaliable (+ line-index 1) (- column-index 2) node)
   (successor-avaliable (+ line-index 1) (+ column-index 2) node)
   )
  )

;; substitui uma posição de uma lista com um valor enviado por parametro	

;; (replace-position 2 ‘(31 32 33 34 35 36 37 38 39 40))

;; (31 32 NIL 34 35 36 37 38 39 40)
(defun replace-position (column-index line &optional (val nil))
  (cond
   ( (null line) '())
   ( (eq column-index 0) (cons val (cdr line)))
   ( (cons (car line) (replace-position (- column-index 1) (cdr line) val))))
  )


;; substitui uma posição de um tabuleiro com um valor enviado por parametro	

;; (replace-value 5 5 ‘((NIL 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; ((NIL 2 3 4 5 6 7 8 9 10) 
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30) 
;; (31 32 33 34 35 36 37 38 39 40) 
;; (41 42 43 44 45 46 47 NIL 49 50) 
;; (51 52 53 54 55 NIL 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90) 
;; (91 92 93 94 95 96 97 98 99 100))
(defun replace-value (line-index column-index graph &optional (val nil))
  (cond
   ( (null graph) '())
   ( (eq line-index 0) (cons (replace-position column-index (nth line-index graph) val) (cdr graph)))
   ( (cons (car graph) (replace-value (- line-index 1) column-index (cdr graph) val))))
  )

;; substitui o valor de um nó por nil

;; (remove-node 55 ‘((NIL 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; ((NIL 2 3 4 5 6 7 8 9 10) 
;; (11 12 13 14 15 16 17 18 19 20) 
;; (21 22 23 24 25 26 27 28 29 30) 
;; (31 32 33 34 35 36 37 38 39 40) 
;; (41 42 43 44 45 46 47 NIL 49 50) 
;; (51 52 53 54 NIL 56 57 58 59 60) 
;; (61 62 63 64 65 66 67 68 69 70) 
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100))

(defun remove-node (value board)
		
  (let* (
         (line-column (successor-position value board))
         )
    
    (cond ((or (null (first line-column)) (null (second line-column))) board)
          (t (replace-value (first line-column) (second line-column) board))
          )
    )
  )

;; remove o nó com valor simetrico ao nó do parametro ou remove o nó com menor valor

;; (remove-dimetric 13 ‘((NIL 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; ((NIL 2 3 4 5 6 7 8 9 10) 
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (NIL 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60) 
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100))
(defun remove-simetric (value board &optional (strategy 'max))
  (let* (
         (simetric (reverse (write-to-string value)))
         )
   
    (cond 	((< (nbDigits value) 2) (node-state-board board))
			((equal (parse-integer simetric)  value) (remove-node (remove-2digit-node board strategy) board))		
			(t (remove-node (parse-integer simetric) board))
          )
    ) 
  )


;; busca o menor valor do grafo

;; (min-node ‘((NIL 2 3 4 5 6 7 8 9 10)
;; (11 12 13 14 15 16 17 18 19 20)
;; (21 22 23 24 25 26 27 28 29 30)
;; (31 32 33 34 35 36 37 38 39 40)
;; (41 42 43 44 45 46 47 NIL 49 50)
;; (51 52 53 54 55 56 57 58 59 60)
;; (61 62 63 64 65 66 67 68 69 70)
;; (71 72 73 74 75 76 77 78 79 80)
;; (81 82 83 84 85 86 87 88 89 90)
;; (91 92 93 94 95 96 97 98 99 100)))

;; 2



(defun min-node (graph)
  (apply 'min
         (apply #'append
                (mapcar
                 (lambda (lin) 
                   (apply #'append
                          (mapcar (lambda (n) (cond
                                               ((null n) n)
                                               (t (list n))
                                               ))
                                  lin  )
                          )
                   )
                 graph))
         ))

		 
(defun remove-2digit-node (graph &optional (strategy 'max))
;;function to remove a 2 digit number from the board
;;strategy is the function to apply: max, min, etc
  (apply strategy
         (apply #'append
                (mapcar
                 (lambda (lin) 
                    (apply #'append
                     (mapcar 
                        (lambda (n) 
                            (cond
                              ((or (null n) (< (nbDigits n) 2)) nil)
                              ((not(equal (parse-integer (reverse (write-to-string n)))  n)) nil)		
                
                              (t (list n))
                            )
                        )
                      lin )
                     )
                 )
				graph)
		  )
         ))
;;test: (remove-2digit-node ‘(
;;(1 2 3 4 5 6 7 8 9 10)
;;(11 12 13 14 15 16 17 18 19 20)
;;(21 22 23 24 25 26 27 28 29 30)
;;(31 32 33 34 35 36 37 38 39 40)
;;(41 42 43 44 45 46 47 48 49 50)
;;(51 52 53 54 55 56 57 58 59 60)
;;(61 62 63 64 65 66 67 68 69 70)
;;(71 72 73 74 75 76 77 78 79 80)
;;(81 82 83 84 85 86 87 88 89 90)
;;(91 92 93 94 95 96 NIL NIL NIL 100)
;;))
;;result: 88

;;test2: (remove-2digit-node ‘(
;;(1 2 3 4 5 6 7 8 9 10)
;;(11 12 13 14 15 16 17 18 19 20)
;;(21 22 23 24 25 26 27 28 29 30)
;;(31 32 33 34 35 36 37 38 39 40)
;;(41 42 43 44 45 46 47 48 49 50)
;;(51 52 53 54 55 56 57 58 59 60)
;;(61 62 63 64 65 66 67 68 69 70)
;;(71 72 73 74 75 76 77 78 79 80)
;;(81 82 83 84 85 86 87 88 89 90)
;;(91 92 93 94 95 96 NIL NIL NIL 100)
;;)'min)
;result: 11

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