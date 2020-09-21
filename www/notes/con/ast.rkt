#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(if (zero? ,Expr) ,Expr ,Expr)

(struct int-e (i))
(struct add1-e (e))
(struct sub1-e (e))
(struct if-e (i t f))

(define (ast->sexpr a)
  (match a
    [(int-e i)    `(int-e ,i)]
    [(add1-e e)   `(add1-e ,(ast->sexpr e))]
    [(sub1-e e)   `(sub1-e ,(ast->sexpr e))]
    [(if-e i t f) `(if-e (zero? ,(ast->sexpr i))
                         ,(ast->sexpr t)
                         ,(ast->sexpr f))]))
