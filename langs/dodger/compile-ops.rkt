#lang racket
(provide compile-op1)
(require "ast.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define r9  'r9)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (seq
          (Cmp rax 0)
          if-equal)]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    ['char->integer
     (seq
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]))


;; -> Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

