#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../run.rkt")

(test-runner    (λ (e) (run (compile (parse e)))))
(test-runner-io (λ (e s) (run/io (compile (parse e)) s)))
