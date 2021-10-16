#lang racket
(define (repeat n v)
  (if (zero? n)
      '()
      (cons v (repeat (sub1 n) v))))

(repeat 10 42)

