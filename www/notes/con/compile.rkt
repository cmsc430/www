#lang racket
(provide compile compile-e)
(require "ast.rkt" "asm/ast.rkt")

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
          (Sub 'rax 1))]
    [(IfZero e1 e2 e3)
     (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
       (seq (compile-e e1)
            (Cmp 'rax 0)
            (Je l1)
            (compile-e e3)
            (Jmp l2)
            (Label l1)
            (compile-e e2)
            (Label l2)))]))