#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(test-runner    (λ (e) (unload/free (asm-interp (compile (parse e))))))
(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    ['err 'err]
                    [(cons r o) (cons (unload/free r) o)])))
