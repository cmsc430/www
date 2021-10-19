#lang racket
(provide (all-defined-out))
(require "interp.rkt" "compile.rkt" "types.rkt" a86 rackunit)
 
(define (check-compiler e)
  (check-eqv? (match (asm-interp (compile e))
                ['err 'err]
                [b (bits->value b)])
              (interp e)
              e))
