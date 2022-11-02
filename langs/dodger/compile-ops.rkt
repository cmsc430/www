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
          (Mov rax val-false)
          (Mov r9 val-true)
          (Cmove rax r9))]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          (Mov rax val-false)
          (Mov r9 val-true)
          (Cmove rax r9))]
    ['char->integer
     (seq (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]))
