#lang racket
(require "test-runner.rkt"
         "build-runtime.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

(test-runner    (λ (e) (unload/free (asm-interp (compile (parse e))))))
(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    ['err 'err]
                    [(cons r o) (cons (unload/free r) o)])))
