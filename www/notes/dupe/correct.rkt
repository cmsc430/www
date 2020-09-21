#lang racket
(provide (all-defined-out))
(require rackunit
         "interp.rkt"
         "compile.rkt"
         "syntax.rkt"
         "asm/interp.rkt")

(define (check-correctness e)
  (with-handlers ([exn:fail? void])
    (let ((e (sexpr->ast e)))
      (check-eqv? (interp e)
                  (asm-interp (compile e))))))
