#lang racket
(require "../random.rkt"
         (only-in "../interp.rkt" interp)
         (only-in "../compile.rkt" compile)
         "../asm/interp.rkt"
         "../syntax.rkt"
         rackunit)
 
(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))

;; boy is this slow
(for ([i (in-range 100)])
  (check-compiler (sexpr->ast (random-expr))))
