#lang racket
(begin
  (define (id z) z)
  (define (f y) (+ y y))
  (define (sum acc n) (if (zero? n)
                          acc
                          (sum (+ n acc) (sub1 n))))
  (sum 0 100000))
