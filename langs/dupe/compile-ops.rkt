#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax)
(define r9 'r9)   ; scratch

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))
