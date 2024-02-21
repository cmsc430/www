#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
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
    [(Lit i) (seq (Mov rax i))]
    [(Prim1 p e) (compile-prim1 p e)]
    [(IfZero e1 e2 e3)
     (compile-ifzero e1 e2 e3)]))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3)
  (let ((l1 (gensym 'ifz))
        (l2 (gensym 'ifz)))
    (seq (compile-e e1)
         (Cmp rax 0)
         (Jne l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

