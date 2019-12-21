;;load files
(defvar *base-pathname* (or *load-truename* *compile-file-truename*))

(defun asset-path (file) (merge-pathnames file *base-pathname*))

(progn
  (load (asset-path "puzzle.lisp"))
  (load (asset-path "search.lisp")))
;;=========load files end=============;

(defun start-game()
  (let (
        (mode (read-mode))
        (algorithm (read-algorithm))
        )

    (cond      
     ((equal mode 'problems)         
      (cond ((equal algorithm 'DFS)
             (dfsearch (read-problem))  
             )
            ((equal algorithm 'BFS)
             (bfsearch (read-problem))  
             )
            ((equal algorithm 'A*)
             (a*search (read-problem) (read-heuristic))   
             )
            )
      )
     ((equal mode 'exercises) 
      (cond ((equal algorithm 'DFS)
             (dfsearch (read-exercise))  
             )
            ((equal algorithm 'BFS)
             (bfsearch (read-exercise))  
             )
            ((equal algorithm 'A*)
             (a*search (read-exercise) (read-heuristic))  
             )
            )
      )  
            
     (T (format t "Thankyou for Playing!") 
        t))
    ))


;; =================================read algorithm=============================================
(defun read-algorithm()
"Allows to make the reading of the algorithm to use"
  (progn
    (show-algorithm)
    (let ((answer (read)))
      (cond ((= answer 1) 'DFS)
            ((= answer 2) 'BFS)
            ((= answer 3) 'A*)
            ((= answer 4) nil)
            (T (format t "Insert a valid option please!") 
              read-algorithm())))
    )
  )

(defun show-algorithm()
  "Reads the algorithm choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
    (terpri)
    (format t "   ~%|                Choose an algorithm:          	    		  |")
    (terpri)
    (format t "   ~%|                1 - Depth-First                          |")
    (format t "   ~%|                2 - Breadth-First                        |")
    (format t "   ~%|                3 - A*                                   |")
    (terpri)
    (format t "   ~% ---------------------------------------------------------~%~%> ")
    )
  )
;; =================================read algorithm END=============================================

;; =================================read depth=============================================
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
;; =================================read depth end=============================================
	
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

;;read start position	
(defun read-start-position()
  "Allows to make a reading of the horses start column."
	
  (list (read-start-line) (read-start-column))
  )

;;read desired points
(defun read-target-points()
  "Allows to make a reading of the target number of points."
	
  (progn
    (format t "What is the target point number? ~%")
    (read)
    ))
	

;; =================================read heuristic=============================================
(defun show-heuristics()
  "Reads the heuristic choice"
  (format t "    ~%---------------------HORSE GAME---------------------------")
  (terpri)
  (format t "   ~%|                Choose an heuristic:          			|")
  (terpri)
  (format t "   ~%|                1 - Sugested Heuristic                   |")
  (format t "   ~%|                2 - Created Heuristic                    |")
  (format t "   ~%|                Other - Back to Start                    |")
  (format t "   ~% ---------------------------------------------------------~%~%> ")
  )


(defun read-heuristic()
  "Read the chosen heuristic"

	(show-heuristics)
  (let ((heuristic-option (read)))
    (cond  
           ((or (not (numberp heuristic-option)) (< heuristic-option 0)) 
            (progn (format t "Insert a valid option!")) (read-heuristic))
           ((eq heuristic-option 1) 'heuristic-sugested)
           ((eq heuristic-option 2) 'heuristic-created)
           (progn (format t "Insert a valid option!")) (read-heuristic))
           )
)
 ;; =================================read heuristic END============================================= 
	
	
(defun show-mode()
  "Reads the mode choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
    (terpri)
    (format t "   ~%|                Choose a Mode:          		        		  |")
    (terpri)
    (format t "   ~%|                1 - Exercises                            |")
    (format t "   ~%|                2 - Problems                             |")
    (format t "   ~%|                3 - LEAVE                                |")
    (terpri)
    (format t "   ~% --------------------------------------------------------~%~%> ")
    )
  )

(defun read-mode()
  (progn
    (show-mode)
    (let ((answer (read)))
      (cond ((eq answer 1) 'exercises);;GO TO EXERCISES
            ((eq answer 2) 'problems)
            ((eq answer 3) (quit));leave
            (T (format t "Thankyou for playing!") 
               )))
    )
  )


;;;READ PROBLEMS
(defun problems-file-path()
"gets the path of the problems file"
    (asset-path "problems.dat")
)

(defun read-boards ()
  "Gets the boards in the problems.dat file"
  (with-open-file (file (problems-file-path) :if-does-not-exist nil)
    (do ((result nil (cons next result))
         (next (read file nil 'eof) (read file nil 'eof)))
        ((equal next 'eof) (reverse result))
      )
    )
  )

(defun choose-problem(&optional(i 1) (problems (read-boards)))	
  "Ilustra as boards no ficheiro problemas.dat"
  (cond
   ((null problems) 
    (progn                                
      (format t "~% -----------------------------------------------------------~%~%>"))
    )
   (T (progn 
        (cond ((= i 1) (progn (format t "~% -----------------------------------------------------------")
                         (format t "~%|               HORSE GAME              |")
                         (terpri)
                         (format t "~%|             Choose a Problem          |")
                         (terpri)
                         )))
        (format t "~%|         ~a - Board number: ~a          |" i i) 
        (choose-problem (+ i 1) (cdr problems))))
   )
  )

;;<board>::= board/table
(defun read-problem()
"Allows user to select a board from the problems file"

    (progn 
      (choose-problem)
      (let ((problem-option (read)))
           (cond 
                 ((not (numberp problem-option)) 
                      (progn (format t "Insert a valid option Please") (read-problem))) 
                 (T
                  (let ((boards-list (read-boards)))
                  (cond (
                        (or (< problem-option 0) (> problem-option (length boards-list))) 
                            (progn (format t "Insert a valid option Please") (read-problem)))
                        ((eq problem-option 0)  nil)
                        (t (nth (1- problem-option) boards-list))
                  )))
           )
       )
    )
)
;;;;;;END READ PROBLEMS


(defun show-exercises()
"Reads the algorithm choice"
  (progn
    (format t "    ~%---------------------HORSE GAME---------------------------")
	(terpri)
	(format t "  ~%-|||||||||||||||||||||EXERCISES|||||||||||||||||||||||||||")
	(terpri)
  (format t "  ~%|                    Choose a Board:          		  	    |")
    (terpri)
  (format t "  ~%|              	  	 1 - Board A                        |")
  (format t "  ~%|                 	   2 - Board B                        |")
	(format t "  ~%|               	     3 - Board C                        |")
	(format t "  ~%|              	  	 4 - Board D                        |")
	(format t "  ~%|              		   5 - Board E                        |")
	(format t "  ~%|              	     6 - Board F                        |")
    (terpri)
    (format t "   ~% --------------------------------------------------------~%~%> ")
  )
)

(defun read-exercise()
  
  (progn 
    (show-exercises)
    (let ((opt (read)))
      (cond 
       ((eq opt 1) (board-a))
       ((eq opt 2) (board-b))
       ((eq opt 3) (board-c))
       ((eq opt 4) (board-d))
       ((eq opt 5) (board-e))
       ((eq opt 6) (board-f))
       (t (progn (format t "Thankyou for playing!") t))
       )          
      )
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

  (cond (
    (null solution-node) nil)
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
  "lista a board"
  (not (null (mapcar #'(lambda(l) (format stream "~%~t~t ~a" l)) board)))
  )

(defun statistics-file-path()
  "gets the path for the local statistics file"
  (asset-path "statistics.dat")
  )

