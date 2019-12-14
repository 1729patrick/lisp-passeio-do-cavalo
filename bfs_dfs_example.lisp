;; bfs1.lisp
;; by Kerstin Voigt, after Paul Graham, pp. 52, 
;; but simpler plain breadth-first search, paths;

;; assume the following:
;; (1) graph is a list of sublists (n1 n2 n3 ... nk) where
;; n2 ... nk are neighbours of n1; graph is DIRECTED
;; and ACYCLIC (for now);
;; (2) start is the start node of search
;; (3) goal is the goal node to be reached;
;; (4) open is a list of nodes whose successors have
;;     not yet been "tried" yet;

;; search tries to move from start to goal
;; with plain breadth-first search;

(defun bfssearch (start goal graph)
  (plain-bfs goal (list start) graph)
  )

;; plain bfs;
(defun plain-bfs (goal open graph)
  (if (null open)
      nil
    (let ((node (car open))
	  )
      
      (terpri) ; old style newline;
      (format t "NODE: ~A   OPEN: ~A" node open)
      (terpri)
      
      (if (eql node goal)
	  goal
	(plain-bfs goal 
		   (append (cdr open)
			   (successors-bfs node graph))
		   graph)
	))
    ))

;; list of successors of node in graph
(defun successors-bfs (node graph)
  (cdr (assoc node graph)))


;; dfs2.lisp
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
;; with plain breadth-first search;

(defun dfssearch (start goal graph)
  (path-dfs goal (list (list start)) graph)
  )

;; plain dfs;
(defun path-dfs (goal open graph)
  (if (null open)
      nil
    (let* ((path (car open))
	   (node (car path))
	   )
      
      (terpri)
      (format t "OPEN: ~A PATH: ~A  NODE: ~A" open path node)
      (terpri)
      
      (if (eql node goal)
	  (reverse path)
	(path-dfs goal 
		   (append 
		    (successor-paths-dfs path node graph)
		    (cdr open))
		   graph)
	))
    ))

;; list of successor paths; there are as many successor paths 
;; as neighvors of node in graph; each successor path is the path
;; that results when adding one such neighbors to the current path;

(defun successor-paths-dfs (path node graph)
  (mapcar #'(lambda (x)
	      (cons x path))
	  (cdr (assoc node graph))))

