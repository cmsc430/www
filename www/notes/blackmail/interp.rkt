#lang racket
(provide (all-defined-out))

(require "ast.rkt")
(require "primitives.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(int-e i) i]
    [(add1-e e1) (add1 (interp e1))]
    [(sub1-e e1) (sub1 (interp e1))]
    [(get-i)   (get-int)]))
