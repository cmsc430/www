#lang racket
(require "../random.rkt"
         "../interp.rkt"
         "../compile.rkt"
         "../asm/interp.rkt"
         rackunit)
 
(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))

;; boy is this slow
(for ([i (in-range 100)])
  (check-compiler (random-expr)))
