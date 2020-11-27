#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../asm/interp.rkt")

(test-runner    (λ (e) (asm-interp (compile (parse e)))))
(test-runner-io (λ (e s) (asm-interp/io (compile (parse e)) s)))
