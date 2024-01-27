#lang racket
(provide (all-defined-out))
(require "interp.rkt"
         "compile.rkt"
         "types.rkt"
         "parse.rkt"
         "run.rkt"
         rackunit)


(define (check-compiler e)  
  (check-equal? (run (compile (parse e)))
                (interp (parse e))
                e))
