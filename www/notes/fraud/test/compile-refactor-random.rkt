#lang racket
(provide (all-defined-out))
(require "../interp.rkt"
         "../random.rkt"
         "../compile-refactor.rkt"
         "../asm/interp.rkt"
         rackunit)
 
(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))

(for ([i (in-range 500)])
  (check-compiler (random-expr)))
