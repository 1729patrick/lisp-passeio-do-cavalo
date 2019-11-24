; dfs2.lisp
;; by Kerstin Voigt, after Paul Graham, pp. 52, 
;; collects paths, returns shortest solution path;

;; assume the following:
;; (1) graph is a list of sublists (n1 n2 n3 ... nk) where
;; n2 ... nk are neighbours of n1; graph is DIRECTED
;; and ACYCLIC (for now);
;; (2) start is the start node of search
;; (3) goal is the goal node to be reached;
;; (4) open is a list of nodes whose successors have
;;     not yet been "tried" yet;

;; search tries to move from start to goal
;; with plain depth-first search;

(defun dfsearch (start goal graph)
  (path-dfs goal  (list start) graph)
  )

;; plain dfs;
(defun path-dfs (goal open graph)
  (if (null open)
      nil
    (let* ((path (car open))
	   (node  path)
	   )
      
      (terpri)
      (format t "OPEN: ~A PATH: ~A  NODE: ~A" open path node)
      (terpri)
      
      (if (eql node goal)
	  ;;(reverse path)
          (format t "terminou")
	(path-dfs goal 
		   (append 
		    (successors-path node graph)
		    (cdr open))
		   graph)
	))
    ))


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
  (let (
        (line (apply #'append
                     (mapcar
                      (lambda (lin) 
                        (cond
                         ((position node lin) lin)
                         (T nil)
                         )
                        )
                      graph
                      ))
              ))
  
    line
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
    (list value)
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


;; a graph for testing: a to b and c, b to c, c to d;
;;(setq graph '((a b c) (b c) (c d)))

(setq graph '((a b c) (b c e) (c d) (d e f) (e f) (f g h) (h g))) 