#lang racket
(require "types.rkt")
(provide interp-prim1-bits)

;; Op Bits -> Bits
(define (interp-prim1-bits op b)
  (match op
    ['add1  (+ b (value->bits 1))]
    ['sub1  (- b (value->bits 1))]
    ['zero? (if (zero? b) (value->bits #t) (value->bits #f))]))
