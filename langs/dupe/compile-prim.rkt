#lang racket
(provide compile-prim1)
(require "types.rkt" a86/ast)

;; Op Asm -> Asm
(define (compile-prim1 p c)
  (seq c
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))])))
