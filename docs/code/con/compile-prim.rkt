#lang racket
(provide compile-prim1)
(require "types.rkt" a86/ast)

;; Op Asm -> Asm
(define (compile-prim1 p c)
  (seq c
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))])))
