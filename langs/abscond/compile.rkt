#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require a86/ast)

(define rax 'rax)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit i) (seq (Mov rax i))]))

