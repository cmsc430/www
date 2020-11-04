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
             (list (Sub 'rax 1)))]
    [(IfZero e1 e2 e3)
     (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
       (append (compile-e e1)
               (list (Cmp 'rax 0)
                     (Je l1))
               (compile-e e3)
               (list (Jmp l2)
                     (Label l1))
               (compile-e e2)
               (list (Label l2))))]))