#lang racket
(provide (all-defined-out))
(require "ast.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Prim p e) (compile-prim p e)]
    [(Int i)    (compile-integer i)]))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax 1)]
         ['sub1 (Sub 'rax 1)])))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax i)))
