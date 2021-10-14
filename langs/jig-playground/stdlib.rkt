#lang racket
(provide reverse)

(define (rev/acc xs a)
  (if (empty? xs)
      a
      (rev/acc (cdr xs) (cons (car xs) a))))

(define (reverse xs)
  (rev/acc xs '()))

