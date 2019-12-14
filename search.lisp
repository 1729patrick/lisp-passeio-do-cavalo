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

(defun dfsearch (start graph)
  (dfs (list (list start)) graph)
  )

(defun bfsearch (start graph)
  (bfs (list start) '()  graph)
  )


;; algoritmo de busca dfs

;; (dfs ‘(55) ‘()‘((1 2 3 4 5 6 7 8 9 10)
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
(defun dfs (open graph)
  (cond ((null open) nil)
        (t 
         (let* ((path (car open))
                (node (car path))
                (successors (successors-path path node graph))
                )
						 
           (cond 
            ((null successors)
             (terpri)
             (format t "~A" (reverse path))
             (terpri)
             )
            (t (dfs 
                (append successors (cdr open))
                (remove-simetric node (remove-node node graph))
                ))
            ))
         )
        )
  )

(defun bfs (open closed graph)
  (cond ((null open) nil)
        (t 
         (let* ((node (car open))
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
            ((nul (member node closed)) (bfs 
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
(defun successor-position (node graph)
  (let* (
         (line (successor-line node graph))
         )

    (list (position line graph :test #'equal) (position node line :test #'equal))
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
(defun successor-avaliable (line-index column-index graph)
  (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   (t (successor-value line-index column-index graph))
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
(defun successors (line-index column-index graph)
  (append
   (successor-avaliable (- line-index 2) (- column-index 1) graph)
   (successor-avaliable (- line-index 2) (+ column-index 1) graph)
   (successor-avaliable (+ line-index 2) (- column-index 1) graph)
   (successor-avaliable (+ line-index 2) (+ column-index 1) graph)
   (successor-avaliable (- line-index 1) (- column-index 2) graph)
   (successor-avaliable (- line-index 1) (+ column-index 2) graph)
   (successor-avaliable (+ line-index 1) (- column-index 2) graph)
   (successor-avaliable (+ line-index 1) (+ column-index 2) graph)
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

(defun remove-node (node graph)
  (let* (
         (line-column (successor-position node graph))
         )
    
    (cond ((or (null (first line-column)) (null (second line-column))) graph)
          (t (replace-value (first line-column) (second line-column) graph))
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
(defun remove-simetric (node graph)
  (let* (
         (simetric (reverse (write-to-string node)))
         )
   
    (cond ((equal (parse-integer simetric)  node) (remove-node (min-node graph) graph))
          (t (remove-node (parse-integer simetric) graph))
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