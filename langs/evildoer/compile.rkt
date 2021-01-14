#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "asm/ast.rkt")

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e)
       (Ret)))

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
    ['read-byte
     (seq (Push 'rbp)
          (Call 'read_byte)
          (Pop 'rbp))]))
          
  
;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ['zero?
          (let ((l1 (gensym)))
            (seq (Cmp 'rax 0)
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
          (seq (Sar 'rax char-shift)
               (Sal 'rax int-shift))]
         ['integer->char
          (seq (Sar 'rax int-shift)
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
          (seq (Push 'rbp)
               (Mov 'rdi 'rax)
               (Call 'write_byte)
               (Pop 'rbp)
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
