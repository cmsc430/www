#lang racket
(provide (all-defined-out))
(require "ast.rkt" a86/ast "compile-prim.rkt")

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Prim1 p e)       (compile-prim1 p (compile-e e))]
    [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax i)))

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax 0)
         (Je l1)
         (compile-e e3)
         (Jmp l2)
         (Label l1)
         (compile-e e2)
         (Label l2))))
