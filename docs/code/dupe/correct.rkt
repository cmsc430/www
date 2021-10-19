#lang racket
(provide (all-defined-out))
(require rackunit
         "interp.rkt"
         "compile.rkt"
         "types.rkt"
         a86/interp)

(define (check-correctness e)
  (with-handlers ([exn:fail? void])
    (check-eqv? (interp e)
                (bits->value (asm-interp (compile e))))))
