#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (compile e)
  `(entry
    (mov rax ,e)
    ret))
