#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" interp)
         (only-in "compile.rkt" compile)
         "asm/interp.rkt"
         "syntax.rkt"
         rackunit)
 
(define (check-compiler e)
  (let ((e (sexpr->ast e)))
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e)))
