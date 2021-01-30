#lang racket
(provide interp)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 'add1 e0)
     (add1 (interp e0))]
    [(Prim1 'sub1 e0)
     (sub1 (interp e0))]
    [(Prim1 'zero? e)
     (zero? (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
