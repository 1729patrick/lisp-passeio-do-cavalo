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
              (append closed-list (list node))
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
                 (current-successors (successors (first horse-pos) (second horse-pos) node  most-positive-fixnum strategy 0 (append closed-list open-list)))
                 )

           (cond 
            ((<= target-points (node-state-point-sum node)) 
                (format-output node "BFS")
                (make-solution-node (reverse (get-solution-path node)) (node-state-board node) open-list closed-list target-points (node-state-point-sum node))
                )         
            (t                 
             (bfs 
              (append (cdr open-list) current-successors)
              (append closed-list (list node))
              target-points
              strategy
              )
             )
            )
           )
         )
        )
  )

(defun a* (open-list closed-list target-points strategy heuristic)
  (cond ((null open-list) (format t "No solution found"))
        (t 
         (let*  (
                 (node (car open-list))
                 (horse-pos (horsep node))
                 (current-board (horsep node))
                 (current-successors (successors (first horse-pos) (second horse-pos) node most-positive-fixnum strategy target-points (append closed-list open-list) heuristic)))

        
           (cond 
            ((<= target-points (node-state-point-sum node)) 
             (format-output node "A*")
             (make-solution-node (reverse (get-solution-path node)) (node-state-board node) open-list closed-list target-points (node-state-point-sum node))
             )


            (t                 
             (a* 
              (order-open (append (cdr open-list) current-successors))
              (append closed-list (list node))
              target-points
              strategy
              heuristic
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
(defun successor-avaliable (line-index column-index node strategy target-points closed-open-list heuristic)
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
           (points (+ (node-state-point-sum node) points-to-sum))
           (h (get-heuristic-fn board-no-horse (- target-points points) points-to-sum strategy heuristic (depth-node node)))
           (f (+ (depth-node node) h))
           )

      (cond ((node-in-open-closed board-to-be closed-open-list) nil) 
            (t (list (make-node board-to-be points node (1+ (depth-node node)) f)))
            )                 
      )
    )
   )
  )


(defun get-heuristic-fn (board points-to-goal node-points strategy heuristic depth)
  (cond 
   ((equal heuristic 1) (heuristic-sugested board points-to-goal node-points strategy depth))
   ((equal heuristic 2) (heuristic-created board points-to-goal node-points strategy depth))
   (t 0)
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
(defun successors (line-index column-index node max-depth strategy &optional (target-points 0) closed-open-list heuristic)
  (cond  (
          (>= (depth-node node) max-depth) nil)           
         (t (append
             (successor-avaliable (- line-index 2) (- column-index 1) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (- line-index 2) (+ column-index 1) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (+ line-index 2) (- column-index 1) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (+ line-index 2) (+ column-index 1) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (- line-index 1) (- column-index 2) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (- line-index 1) (+ column-index 2) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (+ line-index 1) (- column-index 2) node strategy target-points closed-open-list heuristic)
             (successor-avaliable (+ line-index 1) (+ column-index 2) node strategy target-points closed-open-list heuristic)
             )
            )
         )
  )

(defun heuristic-created (board points-to-goal node-points strategy depth)

  (let* (
         (size-board (length (remove-nil-board board)))
         (simmetric-node-points (simmetric-value node-points board strategy))
         )

    (cond 
     ((= size-board 0) 0)
     (t (float (/ (sum-board-points board) (+ depth 1))))
     )     
    )
  )


(defun heuristic-sugested (board points-to-goal node-points strategy deptg)
  (let* (
         (size-board (length (remove-nil-board board)))
         )

    (cond 
     ((= size-board 0) 0)
     (t (float (/ points-to-goal (/ (sum-board-points board) size-board))))
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


(defun simmetric-value (value board &optional (strategy 'max))
  (let* (
         (simmetric (reverse (write-to-string value)))
         )

    (cond 	
     ((< value 10) (* value 10))
     ((equal (parse-integer simmetric) value) (min-max-asymmetric-node board strategy))		
     (t (parse-integer simmetric))
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

(defun compare-f (a b) 
  (< (node-f a) (node-f b))
  )

(defun order-open (open)
  (sort open 'compare-f)
)

(defun node-in-open-closed (board open-closed)
  (cond 
   ((null (car open-closed)) nil)
   ((equal (node-state-board (car open-closed)) board) t)
   (t (node-in-open-closed board (cdr open-closed)))
   )
  )
