#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r9 'r9)   ; scratch

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq (Call 'read_byte))]
    ['peek-byte (seq (Call 'peek_byte))]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (seq (Cmp rax 0)
          (if-equal))]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
	  (if-equal))]
    ['char->integer
     (seq (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          (if-equal))]
    ['write-byte
     (seq (Mov rdi rax)
          (Call 'write_byte))]))

;; -> Asm
;; set rax to #t or #f if comparison flag is equal
(define (if-equal)
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))
