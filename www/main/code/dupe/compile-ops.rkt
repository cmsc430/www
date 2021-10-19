#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (let ((l1 (gensym)))
       (seq (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]))
