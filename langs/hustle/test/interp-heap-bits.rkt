#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../interp-heap-bits.rkt"
         "../interp-io.rkt")

(test (λ (e) (interp (parse e))))

(test/io (λ (e s) (interp/io (parse e) s)))
