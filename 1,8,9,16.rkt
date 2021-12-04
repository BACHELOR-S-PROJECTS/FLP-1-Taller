#lang eopl
;count-occurrences: Sym X S-list -> Int
;Proposito: retorne el n ́umero de ocurrencias de elem en lst. 
(define (count-occurrences elem list)
  (if (eq? list empty)
    0
    (if (list? (car list))
       (+ (count-occurrences elem (cdr list)) (count-occurrences elem (car list)))
       (if (eq? (car list) elem)
           (+ 1 (count-occurrences elem (cdr list)))
           (count-occurrences elem (cdr list))
        )
    )
 )
)

(count-occurrences 'x '((f x) y (((x z) x))))
;3
(count-occurrences 'x '((f x) y (((x z) () x))))
;3
(count-occurrences 'w '((f x) y (((x z) x))))
;0

;mapping: Function X Listof(Int) X Listof(Int) -> Listof(List(Int, Int)) or '()
;Proposito: retorna una lista de pares (a,b), siendo a elemento de
;L1 y b elemento de L2, al aplicar la funcion unaria F con el argumento a, debe arrojar el numero b. Es decir, se debe
;cumplir que F(a) = b.
(define (mapping F L1 L2)
    (cond
        [(and (eq? L1 empty) (eq? L2 empty)) '()]
        [(or (eq? L1 empty) (eq? L2 empty)) "Error"]
        [(eq? (F (car L1)) (car L2)) (cons (cons (car L1) (cons (car L2) '())) (mapping F (cdr L1) (cdr L2)))]
        [else (mapping F (cdr L1) (cdr L2))]
    )
) 

(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;((1 2) (2 4) (3 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;((2 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
;()

;prod-scalar-matriz Listof(List(Int,Int)) X List(Int,Int) -> Listof(List(Int,Int))
;Proposito: Debe retornar el resultado de realizar el producto escalar, es decir, la multiplicacion matriz por vector
(define (prod-scalar-matriz mat vec)
    (cond
        [(eq? mat empty) '()]
        [else (cons (cons (* (caar mat) (car vec)) (cons (* (cadar mat) (cadr vec)) '())) (prod-scalar-matriz (cdr mat) vec))]
    )
)

(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;((2 3) (4 6))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;((2 3) (4 6) (6 9)

;inorder-succesor tree -> Int
;Proposito: retornar el numero mas pequenho de un arbol de busqueda
(define (inorder-succesor arbol)
    (if (eq? (cadr arbol) empty)
        (car arbol)
        (inorder-succesor (cadr arbol))
    )
)
(inorder-succesor '(12 () ()))
;12
(inorder-succesor '(12 (6 () (7 () ())) ()))
;6

;remove-bin-stree tree X Int -> tree
;Proposito: Retorna un nuevo arbol binario de busqueda donde se ha removido el elemento val
(define (remove-bin-stree arbol val)
  (if (eq? (eq? arbol '()) #f)
      (if (eq? (car arbol) val)
         (cond
             [(and (eq? (cadr arbol) empty) (eq? (caddr arbol) empty)) '()]
             [(and (eq? (eq? (cadr arbol) empty) #f) (eq? (eq? (caddr arbol) empty) #f))
               (append (list (inorder-succesor (caddr arbol))) (cons (remove-bin-stree (cadr arbol) val) '())
                                                               (cons (remove-bin-stree (caddr arbol) (inorder-succesor (caddr arbol))) '()))
             ]
             [(eq? (eq? (cadr arbol) empty) #f) (remove-bin-stree (cadr arbol) val)]
             [(eq? (eq? (caddr arbol) empty) #f) (remove-bin-stree (caddr arbol) val)]
         )
         (append (list (car arbol)) (cons (remove-bin-stree (cadr arbol) val) '()) (cons (remove-bin-stree (caddr arbol) val) '()))
      )
      '()
  )
)

(remove-bin-stree '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))
20)
;(14 (7 () (12 () ()))
;(26 (17 () ())
;(31 () ())))
(remove-bin-stree '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))
26)
; mi output: (14 (7 () (12 () ())) (31 (20 (17 () ()) ()) ()))
; output curso (creo que incorrecto): (14 (7 () (12 () ())) (31 (20 (17 () ()) ()))
