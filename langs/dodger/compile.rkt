#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         a86/ast)

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)       (compile-value i)]
    [(Bool b)      (compile-value b)]
    [(Char c)      (compile-value c)]
    [(Prim p e)    (compile-prim p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov 'rax (value->bits v))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ['zero?
          (let ((l1 (gensym)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['char?
          (let ((l1 (gensym)))
            (seq (And 'rax mask-char)
                 (Xor 'rax type-char)
                 (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['char->integer
          (seq (Sar 'rax char-shift)
               (Sal 'rax int-shift))]
         ['integer->char
          (seq (Sar 'rax int-shift)
               (Sal 'rax char-shift)
               (Xor 'rax type-char))]))) 

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
