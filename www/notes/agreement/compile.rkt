#lang racket
(provide (all-defined-out))

(require "ast.rkt")
(require "primitives.rkt")

;; Expr -> Asm
(define (compile e)
  (match e
    [(int-e i)
      `(entry
        (mov rax ,i)
        ret)]
    [(get-i) `(entry
              ,@get-int-asm
              ret)]))
