#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(int-e i) i]
    [(add1-e e0)
     (match (interp e0)
       [i0 (+ i0 1)])]
    [(sub1-e e0)
     (match (interp e0)
       [i0 (- i0 1)])]))
