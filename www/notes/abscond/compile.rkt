#lang racket
(provide (all-defined-out))
(require "asm/printer.rkt")

;; Expr -> Asm
(define (compile-abscond e)
  `(entry
    (mov rax ,e)
    ret))

;; String -> Void
;; Compile contents of given file name and emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read)))
        (unless (integer? p) (error "syntax error" p))
        (display-asm (compile-abscond p))))))
