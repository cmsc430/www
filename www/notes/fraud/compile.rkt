#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "asm/ast.rkt")

(define imm-shift 1) ; lsb = 0 indicates integerness

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
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Var x)           (compile-variable x c)]
    [(Let x e1 e2)     (compile-let x e1 e2 c)]    
    [(Prim p e)        (compile-prim p e c)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3 c)]))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (arithmetic-shift i imm-shift))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (if b #b11 #b01))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((pos (lookup x c)))       
    (seq (Mov 'rax (Offset 'rsp (- (add1 pos)))))))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Mov (Offset 'rsp (- (add1 (length c)))) 'rax)
       (compile-e e2 (cons x c))))

;; Op Expr -> Asm
(define (compile-prim p e c)
  (seq (compile-e e c)
       (match p
         ['add1
          (seq assert-integer
               (Add 'rax (arithmetic-shift 1 imm-shift)))]
         ['sub1
          (seq assert-integer
               (Sub 'rax (arithmetic-shift 1 imm-shift)))]
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq assert-integer
                 (Cmp 'rax 0)
                 (Mov 'rax #b11)
                 (Je l1)
                 (Mov 'rax #b01)
                 (Label l1)))])))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp 'rax #b01)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Id CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (symbol=? x y)
       [#t (length rest)]
       [#f (lookup x rest)])]))

(define assert-integer
  (seq (Mov 'rbx 'rax)
       (And 'rbx 1)
       (Cmp 'rbx 0)
       (Jne 'err)))
