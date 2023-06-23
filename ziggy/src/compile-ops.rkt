#lang crook
{:= B C D0 D1}
(provide (all-defined-out))
(require "ast.rkt")
{:> D0} (require "types.rkt")
(require a86/ast)

(define rax 'rax)
{:> D0} (define r9 'r9)   ; scratch

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    {:> B D0} ['add1 (Add rax 1)]
    {:> B D0} ['sub1 (Sub rax 1)]
    {:> D0}   ['add1 (Add rax (value->bits 1))]
    {:> D0}   ['sub1 (Sub rax (value->bits 1))]
    {:> D0}
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9 (value->bits #t))
          (Cmove rax r9))]
    {:> D1}
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          (Mov rax (value->bits #f))
          (Mov r9 (value->bits #t))
          (Cmove rax r9))]
    {:> D1}
    ['char->integer
     (seq (Sar rax char-shift)
          (Sal rax int-shift))]
    {:> D1}
    ['integer->char
     (seq (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]))
