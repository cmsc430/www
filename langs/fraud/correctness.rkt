#lang racket
(provide (all-defined-out))
(require "interp.rkt"
         "compile.rkt"
         "asm/interp.rkt"
         rackunit)
 
(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))
