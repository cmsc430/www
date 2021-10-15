#lang racket
(provide sum)
(define (sum xs)
  (if (empty? xs)
      0
      (+ (car xs) (sum (cdr xs)))))
