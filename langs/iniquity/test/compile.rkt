#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
(test (λ p (run (compile (apply parse p)))))
(test/io (λ (in . p) (run/io (compile (apply parse p)) in)))

