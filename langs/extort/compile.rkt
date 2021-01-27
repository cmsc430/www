#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (Sub 'rsp 8)
        (compile-e e)
        (Add 'rsp 8)
        (Ret)
        ;; Error handler
        (Label 'err)
        (Call 'raise_error)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)       (compile-value i)]
    [(Bool b)      (compile-value b)]
    [(Char c)      (compile-value c)]
    [(Eof)         (compile-value eof)]
    [(Prim0 p)     (compile-prim0 p)]
    [(Prim1 p e)   (compile-prim1 p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]
    [(Begin e1 e2) (compile-begin e1 e2)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov 'rax (value->bits v))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (match p
    ['void      (seq (Mov 'rax val-void))]
    ['read-byte (seq (Call 'read_byte))]
    ['peek-byte (seq (Call 'peek_byte))]))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (match p
         ['add1
          (seq assert-integer
               (Add 'rax (value->bits 1)))]
         ['sub1
          (seq assert-integer
               (Sub 'rax (value->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq assert-integer
                 (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['char?
          (let ((l1 (gensym)))
            (seq (And 'rax mask-char)
                 (Xor 'rax type-char)
                 (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['char->integer
          (seq assert-char
               (Sar 'rax char-shift)
               (Sal 'rax int-shift))]
         ['integer->char
          (seq assert-codepoint
               (Sar 'rax int-shift)
               (Sal 'rax char-shift)
               (Xor 'rax type-char))]
         ['eof-object?
          (let ((l1 (gensym)))
            (seq (Cmp 'rax val-eof)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['write-byte
          (seq assert-byte
               (Mov 'rdi 'rax)
               (Call 'write_byte)
               (Mov 'rax val-void))])))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

;; Expr Expr -> Asm
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))

(define (assert-type mask type)
  (seq (Mov 'rbx 'rax)
       (And 'rbx mask)
       (Cmp 'rbx type)
       (Jne 'err)))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq assert-integer
         (Cmp 'rax (value->bits 0))
         (Jl 'err)
         (Cmp 'rax (value->bits 1114111))
         (Jg 'err)
         (Cmp 'rax (value->bits 55295))
         (Jl ok)
         (Cmp 'rax (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))
       
(define assert-byte
  (seq assert-integer
       (Cmp 'rax (value->bits 0))
       (Jl 'err)
       (Cmp 'rax (value->bits 255))
       (Jg 'err)))
       
