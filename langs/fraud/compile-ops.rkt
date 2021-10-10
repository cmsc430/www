#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type
(define rsp 'rsp) ; stack

;; Op0 CEnv -> Asm
(define (compile-op0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 CEnv -> Asm
(define (compile-op1 p c)
  (match p
    ['add1
     (seq (assert-integer rax c)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax c)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym)))
       (seq (assert-integer rax c)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char?
     (let ((l1 (gensym)))
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char->integer
     (seq (assert-char rax c)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint c)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-imm val-eof)]
    ['write-byte
     (seq (assert-byte c)
          (pad-stack c)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack c)
          (Mov rax val-void))]))

;; Op2 CEnv -> Asm
(define (compile-op2 p c)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Sub r8 rax)
          (Mov rax r8))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne (error-label c)))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (value->bits #t))
         (Je l)
         (Mov rax (value->bits #f))
         (Label l))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (value->bits 0))
         (Jl (error-label c))
         (Cmp rax (value->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (value->bits 0))
       (Jl (error-label c))
       (Cmp rax (value->bits 255))
       (Jg (error-label c))))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; CEnv -> Label
;; Determine correct error handler label to jump to.
(define (error-label c)
  (match (even? (length c))
    [#t 'raise_error]
    [#f 'raise_error_align]))
