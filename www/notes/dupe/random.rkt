#lang racket
(provide (all-defined-out))

;; Randomly generate an expression
;; Note: this will often generate programs with type errors
(define (random-expr)
  (contract-random-generate
   (flat-rec-contract e
                      #t
                      #f
                      (integer-in #f #f)
                      (list/c 'add1 e)
                      (list/c 'sub1 e)
                      (list/c 'zero? e)
                      (list/c 'if e e e))))

