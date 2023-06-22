#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Liteger
(define (interp e)
  (match e
    [(Lit i) i]
    [(Prim1 p e) (interp-prim1 p (interp e))]))

;; Op Liteger -> Liteger
(define (interp-prim1 op i)
  (match op
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))
