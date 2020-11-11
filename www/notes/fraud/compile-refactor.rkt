#lang racket
(provide (all-defined-out))
(require "ast.rkt" "asm/ast.rkt")

;; type CEnv = [Listof Id]

(define imm-shift        1)
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     #b0)
(define imm-val-true     #b11)
(define imm-val-false    #b01)

;; Expr -> Asm
(define (compile e)
  (append (list (Label 'entry))
          (compile-e e '())
          (list (Ret)
                (Label 'err)
                (Push 'rbp)
                (Call 'error))))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)       (compile-integer i)]
    [(Bool b)      (compile-boolean b)]
    [(Prim p e)    (compile-prim p e c)]
    [(If e1 e2 e3) (compile-if e1 e2 e3 c)] 
    [(Var x)       (compile-var x c)]
    [(Let x e1 e2) (compile-let x e1 e2 c)]))
    
;; Integer -> Asm
(define (compile-integer i)
  (list (Mov 'rax (arithmetic-shift i imm-shift))))

;; Boolean -> Asm
(define (compile-boolean b)
  (list (Mov 'rax (if b imm-val-true imm-val-false))))

;; Op1 Expr CEnv -> Asm
(define (compile-prim p e c)
  (append (compile-e e c)
          assert-integer
          (match p
            ['add1 (list (Add 'rax 2))]
            ['sub1 (list (Sub 'rax 2))]
            ['zero?
             (let ((l1 (gensym 'nzero)))
               (list (Cmp 'rax 0)
                     (Mov 'rax #b11)
                     (Je l1)
                     (Mov 'rax #b01)
                     (Label l1)))])))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (append (compile-e e1 c)
            (list (Cmp 'rax #b01)
                  (Je l1))
            (compile-e e2 c)
            (list (Jmp l2)
                  (Label l1))
            (compile-e e3 c)
            (list (Label l2)))))

;; Id CEnv -> Asm
(define (compile-var x c)
  (let ((pos (lookup x c)))
    (list (Mov 'rax (Offset 'rsp (- (add1 pos)))))))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (append (compile-e e1 c)
          (list (Mov (Offset 'rsp (- (add1 (length c)))) 'rax))
          (compile-e e2 (cons x c))))

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
  (list (Mov 'rbx 'rax)
        (And 'rbx 1)
        (Cmp 'rbx 0)
        (Jne 'err)))
