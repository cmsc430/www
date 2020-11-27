#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "asm/ast.rkt")

(define imm-shift 1) ; lsb = 0 indicates integerness

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim p e)        (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (arithmetic-shift i imm-shift))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (if b #b11 #b01))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (arithmetic-shift 1 imm-shift))]
         ['sub1 (Sub 'rax (arithmetic-shift 1 imm-shift))]
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax #b11)
                 (Je l1)
                 (Mov 'rax #b01)
                 (Label l1)))]))) 

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax #b01)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))
