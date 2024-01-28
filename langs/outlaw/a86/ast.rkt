#lang racket
(provide (all-defined-out))

(struct Text   ())
(struct Data   ())

(struct Global (x))
(struct Label  (x))
(struct Call   (x))
(struct Ret    ())
(struct Mov    (dst src))
(struct Add    (dst src))
(struct Sub    (dst src))
(struct Cmp    (a1 a2))
(struct Jmp    (x))
(struct Je     (x))
(struct Jne    (x))
(struct Jl     (x))
(struct Jle    (x))
(struct Jg     (x))
(struct Jge    (x))
(struct And    (dst src))
(struct Or     (dst src))
(struct Xor    (dst src))
(struct Sal    (dst i))
(struct Sar    (dst i))
(struct Push   (a1))
(struct Pop    (a1))
(struct Lea    (dst x))
(struct Div    (den))

(struct Offset (r i))
(struct Extern (x))

(struct Equ    (x v))
(struct Const  (x))
(struct Dd (x))
(struct Dq (x))
(struct Plus (e1 e2))

;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))

(define registers
  '(cl eax rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

;; Any -> Boolean
(define (register? x)
  (and (memq x registers) #t))

;; Any -> Boolean
(define (exp? x)
  (or (Offset? x)
      (and (Plus? x)
           (exp? (Plus-e1 x))
           (exp? (Plus-e2 x)))
      (symbol? x)
      (integer? x)))

(define offset? Offset?)

;; Any -> Boolean
(define (label? x)
  (and (symbol? x)
       (not (register? x))))

;; Any -> Boolean
(define (instruction? x)
  (ormap (λ (p) (p x))
         (list Text? Data? Global? Label? Extern? Call? Ret? Mov?
               Add? Sub? Cmp? Jmp? Je? Jne? Jl? Jle? Jg? Jge?
               And? Or? Xor? Sal? Sar? Push? Pop? Lea? Div? Equ?
               Dd? Dq?)))
