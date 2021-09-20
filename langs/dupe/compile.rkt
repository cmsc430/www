#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-prim.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)        (compile-value i)]
    [(Bool b)       (compile-value b)]
    [(Prim1 p e)    (compile-prim1 p (compile-e e))]
    [(If e1 e2 e3)  (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value i)
  (seq (Mov 'rax (value->bits i))))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))
