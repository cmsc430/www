#lang racket
(require "../interp.rkt"
         "../interp-io.rkt"         
         "../parse.rkt"
         "test-runner.rkt")

(test-runner (λ (e) (interp (parse e))))
(test-runner-io (λ (e s) (interp/io (parse e) s)))
