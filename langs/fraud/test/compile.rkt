#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")

(test (λ (e) (run (compile (parse e)))))

(test/io (λ (in e) (run/io (compile (parse e)) in)))

