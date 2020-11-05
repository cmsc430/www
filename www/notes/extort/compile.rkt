#lang racket
(provide compile compile-e)
(require "ast.rkt" "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (append (list (Label 'entry))
          (compile-e e)
          (list (Ret)
                (Label 'err)
                (Push 'rbp)
                (Call 'error))))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)  (list (Mov 'rax (* i 2)))]
    [(Bool b) (list (Mov 'rax (if b #b11 #b01)))]
    [(Add1 e1)
     (append (compile-e e1)
             assert-integer
             (list (Add 'rax 2)))]
    [(Sub1 e1) 
     (append (compile-e e1)
             assert-integer
             (list (Sub 'rax 2)))]
    [(Zero? e)
     (let ((l1 (gensym 'nzero)))
       (append (compile-e e)
               assert-integer
               (list (Cmp 'rax 0)
                     (Mov 'rax #b11)
                     (Je l1)
                     (Mov 'rax #b01)
                     (Label l1))))]       
    [(If e1 e2 e3)
     (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
       (append (compile-e e1)
               (list (Cmp 'rax #b01)
                     (Je l1))
               (compile-e e2)
               (list (Jmp l2)
                     (Label l1))
               (compile-e e3)
               (list (Label l2))))]))

(define assert-integer
  (list (Mov 'rbx 'rax)
        (And 'rbx 1)
        (Cmp 'rbx 0)
        (Jne 'err)))
