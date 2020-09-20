#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | Boolean
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(zero? ,Expr)
;; | `(if ,Expr ,Expr ,Expr)

(struct int-e (i))
(struct bool-e (b))
(struct add1-e (e))
(struct sub1-e (e))
(struct zero?-e (e))
(struct if-e (i t f))

(define (ast->sexpr a)
  (match a
    [(int-e i)    `(int-e ,i)]
    [(bool-e b)   `(bool-e ,b)]
    [(add1-e e)   `(add1-e ,(ast->sexpr e))]
    [(sub1-e e)   `(sub1-e ,(ast->sexpr e))]
    [(zero?-e e)  `(zero?-e ,(ast->sexpr e))]
    [(if-e p t f) `(if-e ,(ast->sexpr p)
                         ,(ast->sexpr t)
                         ,(ast->sexpr f))]))
