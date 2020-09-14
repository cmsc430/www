#lang racket
(provide (all-defined-out))

; type expr = int
;           | get-int
;           | add1 expr
;           | sub1 expr
(struct int-e (i))
(struct get-i ())
(struct add1-e (e))
(struct sub1-e (e))

(define (ast->sexpr a)
  (match a
    [(int-e i)  `(int-e ,i)]
    [(add1-e e) `(add1-e ,(ast->sexpr e))]
    [(sub1-e e) `(sub1-e ,(ast->sexpr e))]
    [(get-i)    'get-i]))
