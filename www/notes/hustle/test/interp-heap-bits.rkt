#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../interp-heap-bits.rkt")

(test-runner (λ (e) (interp (parse e))))
