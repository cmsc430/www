#lang racket
(provide (all-defined-out))
(require rackunit
         "interp.rkt"
         "compile.rkt"
         "asm/interp.rkt")

(define (check-correctness e)
  (with-handlers ([exn:fail? void])
    (check-eqv? (interp e)
                (asm-interp (compile e)))))
