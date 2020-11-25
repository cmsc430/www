#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../asm/interp.rkt")

(test-runner (λ (e) (asm-interp (compile (parse e)))))
