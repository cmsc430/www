#lang racket
(provide (all-defined-out))
(require "compile.rkt" "syntax.rkt" "asm/printer.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read)))
        (unless (expr? p) (error "syntax error" p))
        (asm-display (con-compile p))))))
