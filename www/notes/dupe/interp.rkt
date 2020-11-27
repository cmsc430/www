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
    [(Prim 'add1 e0)
     (add1 (interp e0))]
    [(Prim 'sub1 e0)
     (sub1 (interp e0))]
    [(Prim 'zero? e)
     (zero? (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
