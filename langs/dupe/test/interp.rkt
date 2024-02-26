#lang racket
(require "../interp.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")

(test (λ (e) (interp (parse e))))

