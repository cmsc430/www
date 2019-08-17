#lang racket
(provide (all-defined-out))
(require "asm/printer.rkt")

;; Expr -> Asm
(define (compile-abscond e)
  `(entry
    (mov rax ,e)
    ret))

(with-input-from-file (vector-ref (current-command-line-arguments) 0)
  (λ ()
    (let ((p (read)))
      (unless (integer? p) (error "syntax error" p))
      (display-asm (compile-abscond p)))))
