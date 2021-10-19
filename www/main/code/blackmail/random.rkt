#lang racket
(provide (all-defined-out))
(require "parse.rkt")

;; Randomly generate a Blackmail expression
(define (random-expr)
  (parse
   (contract-random-generate
    (flat-rec-contract b
                       (list/c 'add1 b)
                       (list/c 'sub1 b)
                       (integer-in #f #f)))))
