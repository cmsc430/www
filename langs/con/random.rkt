#lang racket
(provide (all-defined-out))
(require "parse.rkt")

;; Randomly generate an expression
(define (random-expr)
  (parse
   (contract-random-generate
    (flat-rec-contract e
                       (integer-in #f #f)
                       (list/c 'add1 e)
                       (list/c 'sub1 e)
                       (list/c 'if (list/c 'zero? e) e e)))))

