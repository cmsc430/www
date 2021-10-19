#lang racket

;; Compute the length of the list
(define (len xs)
  (if (empty? xs)
      0
      (add1 (len (cdr xs)))))

(len (cons "a" (cons "b" (cons "c" '()))))
