(defun dfsearch (start graph)
  (path-dfs (list start) '() graph)
  )

(defun path-dfs (open closed graph)
  ;;(terpri)
  ;;(format t "OPEN: ~A" open)
  ;;(terpri)
  ;;(format t "GRAPH ~A" graph)
  ;;(terpri)
  ;;(format t "CLOSED ~A" closed)

  (cond ((null open) nil)
        (t (let* ((node (car open))
                  (successors (successors-path node graph))) 
      
             ;;(terpri)
             ;;(format t "NODE: ~A" node)
             ;;(terpri)
             ;;(format t "SUCCESSORS ~A" successors)
             ;;(format t "~A" closed)
             ;;(terpri)
      
             (cond 
              ((null successors)
               (terpri)
               (format t " CLOSED: ~A " closed)
               (terpri)
               )
              (t (path-dfs (append successors node) (append closed (list node)) 
                           (remove-simetric node (remove-node node graph))
                           ))
              ))
           )
        )
  )


(defun horse-position (graph)
  (successor-position T graph)
  )

(defun successors-path (node graph)
  (let* (
         (line-column (successor-position node graph))
         )
    (successors (first line-column) (second line-column) graph)
     
    )
  )

(defun successor-position (node graph)
  (let* (
         (line (successor-line node graph))
         )

    (list (position line graph :test #'equal) (position node line :test #'equal))
    )
  )


(defun successor-line (node graph)
  (apply #'append
         (mapcar
          (lambda (lin) 
            (cond
             ((position node lin :test #'equal) lin)
             (T nil)
             )
            )
          graph
          )
         )        
  )

(defun successor-avaliable (line-index column-index graph)
  (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   (t (successor-value line-index column-index graph))
   )
  )

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
 

(defun successors (lin col graph)
  (append
   (successor-avaliable (- lin 2) (- col 1) graph)
   (successor-avaliable (- lin 2) (+ col 1) graph)
   (successor-avaliable (+ lin 2) (- col 1) graph)
   (successor-avaliable (+ lin 2) (+ col 1) graph)
   (successor-avaliable (- lin 1) (- col 2) graph)
   (successor-avaliable (- lin 1) (+ col 2) graph)
   (successor-avaliable (+ lin 1) (- col 2) graph)
   (successor-avaliable (+ lin 1) (+ col 2) graph)
   )
  )

;;;fun��o que substitui uma posi�ao de uma lista com um valor enviado por parametro	
(defun replace-position (col line &optional (val nil))
  (cond
   ( (null line) '())
   ( (eq col 0) (cons val (cdr line)))
   ( (cons (car line) (replace-position (- col 1) (cdr line) val))))
  )
;;teste: (substituir-posicao 0 (linha 0 (tabuleiro-teste)))	 
;;resultado: (NIL 25 54 89 21 8 36 14 41 96)
;;teste: (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T)	 
;;resultado: (T 25 54 89 21 8 36 14 41 96)


;;;fun��o que substitui uma posi�ao de um tabuleiro com um valor enviado por parametro	
(defun replace-value (lin col graph &optional (val nil))
  (cond
   ( (null graph) '())
   ( (eq lin 0) (cons (replace-position col (nth lin graph) val) (cdr graph)))
   ( (cons (car graph) (replace-value (- lin 1) col (cdr graph) val))))
  )

(defun remove-node (node graph)
  (let* (
         (line-column (successor-position node graph))
         )
    
    (cond ((or (null (first line-column)) (null(second line-column))) graph)
          (t (replace-value (first line-column) (second line-column) graph))
          )
    )
  )

(defun remove-simetric (node graph)
  (let* (
         (simetric (reverse (write-to-string node)))
         )
   
    (cond ((equal (parse-integer simetric)  node) (remove-node (min-node graph) graph))
          (t (remove-node (parse-integer simetric) graph))
          )
    ) 
  )


(defun min-node (graph)
  (apply 'min
         (mapcar
          (lambda (lin) (apply 'min 
                               (apply #'append
                                      (mapcar (lambda (n) (cond
                                                           ((null n) n)
                                                           (t (list n))
                                                           ))
                                              lin  )
                                      )
                               ) )
          graph))
  )
