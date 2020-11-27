#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../interp.rkt"
         "../interp-io.rkt")

(test-runner (λ (e) (interp (parse e))))

(let ((interp/io (make-interp/io interp)))
  (test-runner-io (λ (e s) (interp/io (parse e) s))))
