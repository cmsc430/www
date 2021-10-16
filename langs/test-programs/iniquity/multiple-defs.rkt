#lang racket
(define (f x) x)
(define (g y) (+ y 5))
(define (h x y z) (+ (- x y) z))
(define (unused x y) 42)
(h (f 5) (g 42) 31)

