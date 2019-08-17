#lang racket
(provide (all-defined-out))
(require "asm/printer.rkt")

;; Expr -> Asm
(define (abscond-compile e)
  `(entry
    (mov rax ,e)
    ret))

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read)))
        (unless (integer? p) (error "syntax error" p))
        (asm-display (abscond-compile p))))))
