#lang racket
(provide (all-defined-out))
(require "interp.rkt"
         "compile.rkt"
         "types.rkt"
         "parse.rkt"
         "unload-bits-asm.rkt"
         a86 rackunit)

(unless (file-exists? "runtime.o")
  (system "make runtime.o"))
(current-objs
 (list (path->string (normalize-path "runtime.o"))))

(define (check-compiler e)  
  (check-equal? (unload/free (asm-interp (compile (parse e))))
                (interp (parse e))
                e))
