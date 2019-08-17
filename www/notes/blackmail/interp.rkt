#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (blackmail-interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (match (blackmail-interp e0)
       [i0 (+ i0 1)])]
    [`(sub1 ,e0)
     (match (blackmail-interp e0)
       [i0 (- i0 1)])]))
