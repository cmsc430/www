#lang racket
(provide (all-defined-out))

;; Randomly generate a Blackmail expression
(define (random-expr)
  (contract-random-generate
   (flat-rec-contract b
                     (list/c 'add1 b)
                     (list/c 'sub1 b)
                     (integer-in #f #f))))
