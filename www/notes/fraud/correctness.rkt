#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" interp)
         (only-in "compile.rkt" compile)
         "asm/interp.rkt"
         rackunit)
 
(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))
