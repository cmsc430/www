#lang racket
(define (tri x)
  (if (zero? x)
      0
      (+ x (tri (sub1 x)))))

(tri 9)

