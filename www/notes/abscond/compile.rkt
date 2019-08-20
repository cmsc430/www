#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (abscond-compile e)
  `(entry
    (mov rax ,e)
    ret))
