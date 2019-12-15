;; read algorithm
(defun read-algorithm()
"Allows to make the reading of the algorithm to use"
  (progn
    (format t "Which algorithm do you to use to search? ~%")
    (format t "1- Breadth First Search ~%")
    (format t "2- Depth First Search ~%")
    (let ((answer (read)))
      (cond ((= answer 1) 'bfs)
            (T 'dfs)))
    )
  )
;; read depth
(defun read-depth()
"Allows the reading of the depth for the dfs algorithm."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))
	
(defun current-time()
"Retorna o tempo actual com o formato (h m s)"
  ;;HORAS-MINUTOS-SEGUNDOS
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)
