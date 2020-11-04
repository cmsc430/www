#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (append (list (Label 'entry))
          (compile-e e)
          (list (Ret))))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i) (list (Mov 'rax i))]
    [(Add1 e1)
     (append (compile-e e1)
             (list (Add 'rax 1)))]
    [(Sub1 e1) 
     (append (compile-e e1)
             (list (Sub 'rax 1)))]))