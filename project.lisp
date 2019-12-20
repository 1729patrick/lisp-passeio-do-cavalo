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
    (format t "What is the limit depth? ~%")
    (read)
    ))

;; read strategy
(defun read-strategy()
  (progn
    (format t "What is strategy for remove asymmetric numbers? ~%")
    (format t "1. MAX ASIMMETRIC")
    (terpri)
    (format t "2. MIN ASIMMETRIC")
    (terpri)
    (cond ((equal (read) 2) 'min)
          (t 'max)
          )
    )
  )

	
;; read line
(defun read-start-line()
  "Allows to make a reading of the horses start line."
  (progn
    (format t "What is the initial line number? ~%")
    (read)
    ))
	
	;; read column
(defun read-start-column()
  "Allows to make a reading of the horses start column."
  (progn
    (format t "What is the initial column number? ~%")
    (read)
    ))
	
(defun read-start-position()
  "Allows to make a reading of the horses start column."
	
  (list (read-start-line) (read-start-column))
  )

(defun read-target-points()
"Allows to make a reading of the target number of points."
	
	(progn
    (format t "What is the target point number? ~%")
    (read)
    ))
	
(defun show-algorithm()
"Reads the algorithm choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
	(terpri)
    (format t "   ~%|                Choose an algorithm:          			  |")
    (terpri)
    (format t "   ~%|                1 - Breadth-First Search                 |")
    (format t "   ~%|                2 - Depth-First Search                   |")
    (format t "   ~%|                3 - A*                                   |")
    (format t "   ~%|                OTHER - Back                             |")
    (terpri)
    (format t "   ~% ---------------------------------------------------------~%~%> ")
  )
)

(defun read-heuristic()
"Reads the heuristic choice"
  (format t "    ~%---------------------HORSE GAME---------------------------")
	(terpri)
  (format t "   ~%|                Choose an heuristic:          			|")
    (terpri)
  (format t "   ~%|                1 - Sugested Heuristic                   |")
  (format t "   ~%|                2 - Created Heuristic                    |")
  (format t "   ~%|                0 - Voltar                               |")
  (format t "   ~% ---------------------------------------------------------~%~%> ")
)


(defun read-heuristic-return()
"Le a heuristica escolhida no menu"
  (if (not (read-heuristic-message))
      (let ((heuristic-option (read)))
         (cond ((eq heuristic-option '0) (exec-search))
               ((or (not (numberp heuristic-option)) (< heuristic-option 0)) 
				(progn (format t "Insert a valid option!")) (read-heuristic))
               ((eq opt 1) 'sugested-heuristic)
               (T 'created-heuristic)
     )))
)
	
	
(defun show-mode()
"Reads the algorithm choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
	(terpri)
    (format t "   ~%|                Choose a Mode:          		  		  |")
    (terpri)
    (format t "   ~%|                1 - Exercises                            |")
    (format t "   ~%|                2 - Problems                             |")
    (format t "   ~%|                OTHER - Back                             |")
    (terpri)
    (format t "   ~% --------------------------------------------------------~%~%> ")
  )
)

(defun show-exercises()
"Reads the algorithm choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
	(terpri)
	(format t "    ~%-||||||||||||||||||||EXERCISES|||||||||||||||||||||||||||")
	(terpri)
    (format t "   ~%|                   Choose a Board:          		  	  |")
    (terpri)
    (format t "   ~%|              	 	 1 - Board A                          |")
    (format t "   ~%|               	 2 - Board B                          |")
	(format t "   ~%|               	 3 - Board C                          |")
	(format t "   ~%|              		 4 - Board D                          |")
	(format t "   ~%|              		 5 - Board E                          |")
	(format t "   ~%|              	     6 - Board F                          |")
    (format t "   ~%|              	     OTHER - Back                         |")
    (terpri)
    (format t "   ~% --------------------------------------------------------~%~%> ")
  )
)

(defun current-time()
"Returns the current time with the format (h m s)"
  ;;Hour-minute-second
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)


(defun write-bfsdfs-statistics (start-board solution-node start-time end-time algorithm)
  "Writes the statistics file with the solution and it's statistic data, for breadth first and depth first algorithms"

  (cond ((null solution-node) nil)
        (t 
         (with-open-file (file (statistics-file-path) :direction :output :if-exists :append :if-does-not-exist :create)
           (progn 
             (terpri)
             (format file "~%~t----: Algorithm: ~a " algorithm)
             (format file "~%~t----:  Starting time: ~a:~a:~a" (first start-time) (second start-time) (third start-time))
             (format file "~%~t----:  Ending Time: ~a:~a:~a" (first end-time) (second end-time) (third end-time))
             (format file "~%~t----:  Number of Generated Nodes: ~a" (generated-nodes-number solution-node))
             (format file "~%~t----:  Number of Expanded Nodes: ~a" (fourth solution-node))
             (format file "~%~t----:  Penetrance Level: ~F" (calc-penetrance solution-node))
             (format file "~%~t----:  Median Branching Factor: ~F" (branching-factor solution-node))
             (if (eq algorithm 'DFS)
                 (format file "~%~t----:  Maximum Depth: ~a" (fifth solution-node)))
             (format file "~%~t----:  Solution Length: ~a" (length (first solution-node)))
             (format file "~%~t----:  Goal Points: ~a"  (sixth solution-node))
             (terpri)
             (format file "~%~t----:  Current Points: ~a"  (seventh solution-node))
             (terpri)
             (format file "~%~t----:  Solution Sequence: ~a"  (first solution-node))
             (terpri)    (format file "~%~t----:  Starting Board:")
             (print-board start-board file)
             (terpri)
             (format file "~%~t----:  Final Board:")
             (print-board (second solution-node) file)
             )
           )
         )
        )
)

(defun print-board(board &optional (stream t))
  "Mostra um tabuleiro bem formatado"
   (not (null (mapcar #'(lambda(l) (format stream "~%~t~t ~a" l)) board)))
)

(defun statistics-file-path()
"Devolve o path para o ficheiro (C:\lisp\resultados.dat)"
    (make-pathname :host "c" :directory '(:absolute "horse-game-results") :name "statistics" :type "dat")
)

