#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim 'add1 e) (add1 (interp e))]
    [(Prim 'sub1 e) (sub1 (interp e))]))
