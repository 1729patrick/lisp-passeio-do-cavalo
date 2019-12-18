;;;; laboratorio7.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Ficha de Laboratório nº7 - Apoio ao 1º projeto
;;;; Autor: Henoch Vitureira


;;; Tabuleiros


(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posição: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 NIL 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)


;;; Exercicios

(defun linha (num table)
	(nth num table)
)
;;teste: (linha 0 (tabuleiro-teste))
;; resultado: (94 25 54 89 21 8 36 14 41 96)

(defun celula (line col table)
	(nth col (linha line table))
)
;;teste: (celula 0 1 (tabuleiro-teste))
;; resultado: 25

(defun lista-numeros (&optional (n 100))
  (if (< n 1) '() (cons (1- n) (lista-numeros(1- n))))
)
;;teste: (lista-numeros)
;; resultado: (99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 ... 5 4 3 2 1 0)
 
 ;;remover todas as instancias de um numero numa lista
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista)))))
)

;;;função que baralha uma lista de numeros utilizando rotação entre o primeiro elemento da lista e outro aleatorio
(defun baralhar (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
	(rotatef (car input-list) (nth (random (length input-list)) input-list))
	(baralhar (cdr input-list) 
				 (append accumulator (list (car input-list))))
				 )))
;;teste: (baralhar (lista-numeros))
;;resultado: outputs muito variados
	
;;;função que gera um tabuleiro aleatorio	
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
	(cond
		((null lista) nil)
		(t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
	)
)
;;teste: (tabuleiro-aleatorio)	 
;;resultado: outputs muito variados->   ((3 9 82 94 14 54 86 39 71 97) 
										;;(61 90 42 84 93 62 51 96 45 38) 
										;;(19 76 0 17 37 1 60 40 31 30) 
										;;(67 99 27 55 32 95 57 28 24 6) 
										;;(72 33 79 81 49 73 36 69 16 43) 
										;;(89 66 56 65 48 4 75 47 91 87) 
										;;(68 64 22 53 70 41 50 15 5 52) 
										;;(29 77 35 26 13 34 74 2 44 21) 
										;;(59 7 18 78 88 8 85 83 25 11) 
										;;(58 20 10 63 92 80 23 12 46 98))
										
										
;;;função que substitui uma posiçao de uma lista com um valor enviado por parametro	
(defun substituir-posicao (index lista &optional (val nil))
  (cond
    ( (null lista) '())
    ( (eq index 0) (cons val (cdr lista)))
    ( (cons (car lista) (substituir-posicao (- index 1) (cdr lista) val))))
)
;;teste: (substituir-posicao 0 (linha 0 (tabuleiro-teste)))	 
;;resultado: (NIL 25 54 89 21 8 36 14 41 96)
;;teste: (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T)	 
;;resultado: (T 25 54 89 21 8 36 14 41 96)


;;;função que substitui uma posiçao de um tabuleiro com um valor enviado por parametro	
(defun substituir (index1 index2 board &optional (val nil))
  (cond
    ( (null board) '())
    ( (eq index1 0) (cons (substituir-posicao index2 (linha index1 board) val) (cdr board)))
    ( (cons (car board) (substituir (- index1 1) index2 (cdr board) val))))
)
;;teste: (substituir 0 0 (tabuleiro-teste) T)
;;resultado: (
;(T 25 54 89 21 8 36 14 41 96)
;(78 47 56 23 5 49 13 12 26 60)
;(0 27 17 83 34 93 74 52 45 80)
;(69 9 77 95 55 39 91 73 57 30)
;(24 15 22 86 1 11 68 79 76 72)
;(81 48 32 2 64 16 50 37 29 71)
;(99 51 6 18 53 28 7 63 10 88)
;(59 42 46 85 90 75 87 43 20 31)
;(3 61 58 44 65 82 19 4 35 62)
;(33 70 84 40 66 38 92 67 98 97)
;)

;;devolver posiçao do cavalo
