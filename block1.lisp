;№15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.
(defun cross-prod(v1 v2)
      (cond
          ((null v1) 0)
          (t (+ (* (car v1) (car v2)) (cross-prod(cdr v1) (cdr v2))))
       )
)
(print(cross-prod '(4 2 3) '(7 3 7)))
;55

;№21 
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
(defun del-elem(lst)
      (cond
         ((null lst)  nil)
          (t(cdr lst))
       )
)
(print(del-elem '(2 8 5 3)))
;(8 5 3) 
            
;№22
;Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).
(defun level-list(lst)
      (cond
         ((null(cdr  lst)) (cons (car lst) ()))
          ((list (level-list (cdr lst)) (car lst) ))
       )
)
(print(level-list '( 1 2  3 4 5 6  7 8  9 )))
;(((((((((9) 8) 7) 6) 5) 4) 3) 2) 1) 

;№18
;Определите предикат, проверяющий, является ли аргумент одноуровневым списком.
(defun check-list(lst)
      (cond
         ((null lst) "список одноуровневый")
          ((atom (car lst))(check-list(cdr lst)))
          ("список не одноуровневый")
       )
)
(print(check-list '((2))))
(print(check-list '(2 (8) ((7)))))
(print(check-list '(2 8 7)))
;"список не одноуровневый" 
;"список не одноуровневый" 
;"список одноуровневый" 
