#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../interp-defun.rkt"
         "../interp-io.rkt")

(test-runner    (λ p (interp (parse p))))
(test-runner-io (λ (s . p) (interp/io (parse p) s)))
