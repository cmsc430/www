#lang racket
(require "../compile.rkt"
         "../parse.rkt"
         "../types.rkt"
         "test-runner.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(test-runner (λ (e) (match (asm-interp (compile (parse e)))
                      ['err 'err]
                      [bs (bits->value bs)])))

(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    [(cons 'err o) (cons 'err o)]
                    [(cons r o)
                     (cons (bits->value r) o)])))


