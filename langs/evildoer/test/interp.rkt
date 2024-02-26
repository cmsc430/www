#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")

(test (λ (e) (interp (parse e))))

(test/io (λ (in e) (interp/io (parse e) in)))

