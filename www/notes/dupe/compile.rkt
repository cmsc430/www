#lang racket
(provide compile compile-e)
(require "ast.rkt" "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

;; FIXME: compute shift for constants

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)  (seq (Mov 'rax (* i 2)))]
    [(Bool b) (seq (Mov 'rax (if b #b11 #b01)))]
    [(Add1 e1)
     (seq (compile-e e1)
          (Add 'rax 2))]
    [(Sub1 e1) 
     (seq (compile-e e1)
          (Sub 'rax 2))]
    [(Zero? e)
     (let ((l1 (gensym 'nzero)))
       (seq (compile-e e)
            (Cmp 'rax 0)
            (Mov 'rax #b11)
            (Je l1)
            (Mov 'rax #b01)
            (Label l1)))]
    [(If e1 e2 e3)
     (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
       (seq (compile-e e1)
            (Cmp 'rax #b01)
            (Je l1)
            (compile-e e2)
            (Jmp l2)
            (Label l1)
            (compile-e e3)
            (Label l2)))]))
