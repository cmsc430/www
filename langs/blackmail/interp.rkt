#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Lit i) i]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]))

