#lang racket
(provide (all-defined-out))
(require "ast.rkt" "asm/ast.rkt")

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e '())
       (Ret)
       (Label 'err)
       (Push 'rbp)
       (Call 'error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)  (seq (Mov 'rax (* i 2)))]
    [(Bool b) (seq (Mov 'rax (if b #b11 #b01)))]
    [(Prim p e)
     (seq (compile-e e c)
          assert-integer
          (match p
            ['add1 (Add 'rax 2)]
            ['sub1 (Sub 'rax 2)]
            ['zero?
             (let ((l1 (gensym 'nzero)))
               (seq (Cmp 'rax 0)
                    (Mov 'rax #b11)
                    (Je l1)
                    (Mov 'rax #b01)
                    (Label l1)))]))]
    [(If e1 e2 e3)
     (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
       (seq (compile-e e1 c)
            (Cmp 'rax #b01)
            (Je l1)
            (compile-e e2 c)
            (Jmp l2)
            (Label l1)
            (compile-e e3 c)
            (Label l2)))]
    [(Var x) 
     (let ((pos (lookup x c)))
       (seq (Mov 'rax (Offset 'rsp (- (add1 pos))))))]    
    [(Let x e1 e2)
     (seq (compile-e e1 c)
          (Mov (Offset 'rsp (- (add1 (length c)))) 'rax)
          (compile-e e2 (cons x c)))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (symbol=? x y)
       [#t (length rest)]
       [#f (lookup x rest)])]))

;; Asm
(define assert-integer
  (seq (Mov 'rbx 'rax)
       (And 'rbx 1)
       (Cmp 'rbx 0)
       (Jne 'err)))
