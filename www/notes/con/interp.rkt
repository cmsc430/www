#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Add1 e0)
     (add1 (interp e0))]
    [(Sub1 e0)
     (sub1 (interp e0))]
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]))
