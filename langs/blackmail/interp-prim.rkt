#lang racket
(provide interp-prim1)

;; Op1 Integer -> Integer
(define (interp-prim1 op i)
  (match op
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))

