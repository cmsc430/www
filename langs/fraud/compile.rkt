#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86)

;; Registers used
(define rax 'rax)
(define rbx 'rbx)
(define r8  'r8)
(define rsp 'rsp)
(define rdi 'rdi)

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (compile-e e '())
        (Ret)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)         (compile-value i)]
    [(Bool b)        (compile-value b)]
    [(Char c)        (compile-value c)]
    [(Eof)           (compile-value eof)]
    [(Var x)         (compile-variable x c)]
    [(Prim0 p)       (compile-prim0 p c)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)   (compile-begin e1 e2 c)]
    [(Let x e1 e2)   (compile-let x e1 e2 c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p c)))

;; Op1 CEnv -> Asm
(define (compile-op1 p c)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym)))
       (seq (assert-integer rax)
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
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq assert-codepoint
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (let ((l1 (gensym)))
       (seq (Cmp rax val-eof)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['write-byte
     (seq assert-byte
          (pad-stack c)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack c)
          (Mov rax val-void))]))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p c)))

;; Op2 CEnv -> Asm
(define (compile-op2 p c)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

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

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov rbx arg)
         (And rbx mask)
         (Cmp rbx type)
         (Jne 'raise_error))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (value->bits 0))
         (Jl 'raise_error)
         (Cmp rax (value->bits 1114111))
         (Jg 'raise_error)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'raise_error)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (value->bits 0))
       (Jl 'raise_error)
       (Cmp rax (value->bits 255))
       (Jg 'raise_error)))
