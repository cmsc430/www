#lang racket
(provide compile)
(require "ast.rkt"
         "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i) (seq (Mov 'rax i))]))
