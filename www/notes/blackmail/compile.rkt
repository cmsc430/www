#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i) (seq (Mov 'rax i))]
    [(Add1 e1)
     (seq (compile-e e1)
          (Add 'rax 1))]
    [(Sub1 e1) 
     (seq (compile-e e1)
          (Sub 'rax 1))]))
