#lang racket
(provide compile)
(require "ast.rkt"
         "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (match e
    [(Int i)
     (list (Label 'entry)
           (Mov 'rax i)
           (Ret))]))