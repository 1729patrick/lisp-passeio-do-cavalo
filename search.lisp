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
                  (format-output node "DFS")
                  (make-solution-node (reverse (get-solution-path node)) (node-state-board node) open-list closed-list max-depth)
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
            )
           )
         )
        )
  )

(defun bfs (open-list closed-list target-points strategy)
  (cond ((null open-list) (format t "No solution found"))
        (t 
         (let*  (
                 (node (car open-list))
                 (horse-pos (horsep node))
                 (current-board (horsep node))
                 (current-successors (successors (first horse-pos) (second horse-pos) node  most-positive-fixnum strategy))
                 )

           (cond 
            ((<= target-points (node-state-point-sum node)) 
                (format-output node "BFS")
                (make-solution-node (reverse (get-solution-path node)) (node-state-board node) open-list closed-list)
                )         
            (t                 
             (bfs 
              (append (cdr open-list) current-successors)
              (append node (cdr closed-list))
              target-points
              strategy
              )
             )
            )
           )
         )
        )
  )

(defun a* (open-list closed-list target-points strategy)
  (cond ((null open-list) (format t "No solution found"))
        (t 
         (let*  (
                 (node (max-node-f open-list))
                 (horse-pos (horsep node))
                 (current-board (horsep node))
                 (current-successors (successors (first horse-pos) (second horse-pos) node most-positive-fixnum strategy target-points (append  (list closed-list) (list open-list)))
                 )
)

           (format t "depth ~a" (depth-node node))
           (terpri)
           
           (cond 
            ((<= target-points (node-state-point-sum node)) (format-output node "A*"))
            (t                 
             (a* 
              (append (remove-max-node node open-list) current-successors)
              (append node closed-list)
              target-points
              strategy
              )
             )
            )
           
         )
        )
  )
)
    

(defun horsep (node)
  (successor-position T (node-state-board node))
  )
 
;; busca o index da linha e o index da coluna de um nó
(defun successor-position (value board)
  (let* (
         (line (successor-line value board))
         )

    (list (position line board :test #'equal) (position value line :test #'equal))
    )
  )

;; busca a linha onde está o nó
(defun successor-line (value board)
  (apply #'append
         (mapcar
          (lambda (lin) 
            (cond
             ((position value lin :test #'equal) lin)
             (T nil)
             )
            )
          board
          )
         )        
  )

;; busca o valor de um nó no grafo pelo index da linha e index da coluna ou retorna nil para indexes inválidos
(defun successor-avaliable (line-index column-index node strategy target-points closed-open-list)
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
           (board-no-simmetric (remove-simmetric points-to-sum board-no-horse strategy))
           (board-to-be (replace-value line-index column-index board-no-simmetric T))
           (new-node (make-node board-to-be (+ (node-state-point-sum node) points-to-sum) node (1+ (depth-node node)) (heuristic points-to-sum board-no-horse target-points)))
           )

      (cond ((check-node-in-open-closed (state-node new-node) closed-open-list) nil) 
            (t (list new-node))
            )
      )
    )
   )
  )


;; busca o valor de um nó no grafo pelo index da linha e index da coluna
(defun successor-value (line-index column-index board)
  (let* (
         (line (nth line-index board))
         (value (nth column-index line))
         )
    
    (cond 
     ((null value) nil)
     (t (list value))
     )
    )
  )
 
;; busca todos os sucessores válidos de um nó
(defun successors (line-index column-index node max-depth strategy &optional (target-points 0) closed-open-list)
  (cond  (
          (>= (depth-node node) max-depth) nil)           
         (t (append
             (successor-avaliable (- line-index 2) (- column-index 1) node strategy target-points closed-open-list)
             (successor-avaliable (- line-index 2) (+ column-index 1) node strategy target-points closed-open-list)
             (successor-avaliable (+ line-index 2) (- column-index 1) node strategy target-points closed-open-list)
             (successor-avaliable (+ line-index 2) (+ column-index 1) node strategy target-points closed-open-list)
             (successor-avaliable (- line-index 1) (- column-index 2) node strategy target-points closed-open-list)
             (successor-avaliable (- line-index 1) (+ column-index 2) node strategy target-points closed-open-list)
             (successor-avaliable (+ line-index 1) (- column-index 2) node strategy target-points closed-open-list)
             (successor-avaliable (+ line-index 1) (+ column-index 2) node strategy target-points closed-open-list)
             )
            )
         )
  )

(defun heuristic (g board points-to-goal)
  (let* (
         (size-board  (length (remove-nil-board board)))
         (all-points (sum-board-points board))         
         )

    (cond 
     ((or (= size-board 0) (= g 0)) 0)
     (t (float (+ g (/ points-to-goal (/ all-points size-board)))))
           
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
(defun replace-value (line-index column-index board &optional (val nil))
  (cond
   ( (null board) '())
   ( (eq line-index 0) (cons (replace-position column-index (nth line-index board) val) (cdr board)))
   ( (cons (car board) (replace-value (- line-index 1) column-index (cdr board) val))))
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

;; remove o nó com valor simmetrico ao nó do parametro ou remove o nó com menor valor
  (defun remove-simmetric (value board &optional (strategy 'max))
    (let* (
           (simmetric (reverse (write-to-string value)))
           )
   
      (cond 
       ((null value) board)	
       ((< value 10) (remove-node (* value 10) board))
       ((equal (parse-integer simmetric)  value) (remove-node (min-max-asymmetric-node board strategy) board))		
       (t (remove-node (parse-integer simmetric) board))
       )
      )
    ) 

	 
(defun min-max-asymmetric-node (board &optional (strategy 'max))
  ;;function to remove a 2 digit number from the board
  ;;strategy is the function to apply: max, min, etc
  (let* (
         (min-max (asymmetric-values board))
         )
 
    (cond ((null min-max) nil)
          (t (apply strategy min-max))
          )
    )
  )


(defun asymmetric-values (board)
  (apply #'append
         (mapcar 
          (lambda (n) 
            (cond
             ((or (not (typep n 'real)) (< n 10)) nil)
             ((not(equal (parse-integer (reverse (write-to-string n)))  n)) nil)		
  
             (t (list n))
             )
            )

		(remove-nil-board board)
		)

          
          )
         )
  )

(defun sum-board-points (board)
  (reduce #'+ (remove-nil-board board))
)

(defun remove-nil-board (board)
  (apply #'append
         (mapcar
          (lambda (lin) 
            (apply #'append
                   (mapcar 
                    (lambda (n) 
                      (cond
                       ((or (not (typep n 'real))) nil)
                       (t (list n))
                       )
                      )
                    lin )
                   )
            )
          board)
         )
  )
	
(defun calc-penetrance (solution-node)
"Penetrancia"
  (/ (solution-sequence-length solution-node)  (generated-nodes-number solution-node))
)

(defun max-node-f (open-list &optional node (max -1))
  (cond 
   ((null (car open-list)) node)
   ((> (node-heuristic (car open-list)) max) (max-node-f (cdr open-list) (car open-list) (node-heuristic (car open-list))))
   (t (max-node-f (cdr open-list) node max))
   )
  )


(defun remove-max-node (node open-list &optional new-list)
  (cond ((null open-list) new-list)
        ((equal node (car open-list)) (remove-max-node node (cdr open-list) new-list))
        (t (remove-max-node node (cdr open-list) (append new-list (list node))))
        )
  )

(defun check-node-in-open-closed (state open-closed)
  (cond 
   ((null (car open-closed)) nil)
   ((equal (node-state-board (car open-closed)) state) t)
   (t (check-node-in-open-closed state (cdr open-closed)))
   )
  )
>>>>>>> Stashed changes
