#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]))

