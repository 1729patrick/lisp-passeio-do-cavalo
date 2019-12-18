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
                      (make-successor-node-dfs board-with-horse start-points nil strategy)
                      ))
         (strategy (read-strategy))
         )
    (dfs open-nodes (list nil) (read-target-points) (read-depth) strategy)
    )            
  )

(defun make-successor-node-dfs (board-placed-horse current-points parent &optional strategy)
  (cond 
   ((null parent)
    (make-node (remove-simetric current-point board-placed-horse strategy)
     (+ (node-state-point-sum parent) current-points)
     parent
     0
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
           )
        )
  )

(defun dfs (open-list closed-list target-points max-depth strategy)
  (cond ((null open-list) (format t "No solution found"))
        (t 
           (let*  (
                   (node (car open-list))
                   (horse-pos (horsep node))
                   (current-board (horsep node))
                   (current-successors (successors (first horse-pos) (second horse-pos) node max-depth strategy))
                   )

             (cond 
              ((<= target-points (node-state-point-sum node))
               (terpri)
               ;; SEQUENCIA DE JOGADAS
               (format t "DFS ENDED, POINTS GOTTEN:")
               (terpri)
               (write-line (write-to-string (node-state-point-sum node)))
               (terpri)
               (format t "SOLUTION SEQUENCE:")
               (terpri)
            
             
               (print-list (reverse (get-solution-path node)))
               (terpri)
             
               (format t "FINAL BOARD:")
               (terpri)
               (print-list (node-state-board node))
               (terpri)
               (format t "DEPTH: ~a" (depth-node node))
               )
              (t                 
              (dfs 
                (append current-successors (cdr open-list))
                (append node (cdr closed-list))
                target-points
                max-depth
                strategy
                )
               )
              )))
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
	
;; busca os sucessores de um nó e retornao caminho até os sucessores
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

;; busca o index da linha e o index da coluna de um nó
(defun successor-position (value graph)
  (let* (
         (line (successor-line value graph))
         )

    (list (position line graph :test #'equal) (position value line :test #'equal))
    )
  )


;; busca a linha onde está o nó
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
(defun successor-avaliable (line-index column-index node strategy)
  (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   ((null (car (successor-value line-index column-index (node-state-board node)))) nil)
   (t 
    (let* (
           (horse-pos (horsep node))
           (node-board (node-state-board node))
           (points-to-sum (car (successor-value line-index column-index node-board)))
           (board-no-horse (replace-value (first horse-pos) (second horse-pos) (remove-node points-to-sum node-board)))
           (board-no-simetric (remove-simetric points-to-sum board-no-horse strategy))
           (board-to-be (replace-value line-index column-index board-no-simetric T))   
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
(defun successors (line-index column-index node max-depth strategy)
  (cond  (
          (>= (depth-node node) max-depth) nil)           
         (t (append
             (successor-avaliable (- line-index 2) (- column-index 1) node strategy)
             (successor-avaliable (- line-index 2) (+ column-index 1) node strategy)
             (successor-avaliable (+ line-index 2) (- column-index 1) node strategy)
             (successor-avaliable (+ line-index 2) (+ column-index 1) node strategy)
             (successor-avaliable (- line-index 1) (- column-index 2) node strategy)
             (successor-avaliable (- line-index 1) (+ column-index 2) node strategy)
             (successor-avaliable (+ line-index 1) (- column-index 2) node strategy)
             (successor-avaliable (+ line-index 1) (+ column-index 2) node strategy)
             )
            )
         )
  )

;; substitui uma posição de uma lista com um valor enviado por parametro	
(defun replace-position (column-index line &optional (val nil))
  (cond
   ( (null line) '())
   ( (eq column-index 0) (cons val (cdr line)))
   ( (cons (car line) (replace-position (- column-index 1) (cdr line) val))))
  )


;; substitui uma posição de um tabuleiro com um valor enviado por parametro	
(defun replace-value (line-index column-index graph &optional (val nil))
  (cond
   ( (null graph) '())
   ( (eq line-index 0) (cons (replace-position column-index (nth line-index graph) val) (cdr graph)))
   ( (cons (car graph) (replace-value (- line-index 1) column-index (cdr graph) val))))
  )

;; substitui o valor de um nó por nil
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
(defun remove-simetric (value board &optional (strategy 'max))
  (let* (
         (simetric (reverse (write-to-string value)))
         )
   
    (cond 
     ((null value) board)	
     ((< value 10) (remove-node (* value 10) board))
    ((equal (parse-integer simetric)  value) (remove-node (remove-2digit-node board strategy) board))		
    (t (remove-node (parse-integer simetric) board))
    )
  )
) 


;; busca o menor valor do grafo
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