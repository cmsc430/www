#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "asm/ast.rkt")

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (seq (Label 'entry)
       (compile-e e '())
       (Ret)
       (Label 'err)
       (Push 'rbp)
       (Call 'error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)       (compile-value i)]
    [(Bool b)      (compile-value b)]
    [(Char c)      (compile-value c)]
    [(Eof)         (compile-value eof)]
    [(Var x)       (compile-variable x c)]
    [(Prim0 p)     (compile-prim0 p)]
    [(Prim1 p e)   (compile-prim1 p e c)]
    [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
    [(Begin e1 e2) (compile-begin e1 e2 c)]
    [(Let x e1 e2) (compile-let x e1 e2 c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov 'rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov 'rax (Offset 'rsp i)))))
    
;; Op0 -> Asm
(define (compile-prim0 p)
  (match p
    ['read-byte
     (seq (Push 'rbp)
          (Call 'read_byte)
          (Pop 'rbp))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
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
               (Push 'rbp)
               (Mov 'rdi 'rax)
               (Call 'write_byte)
               (Pop 'rbp)
               (Mov 'rax val-void))])))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp 'rax val-false)
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
       (Mov (Offset 'rsp (next c)) 'rax)
       (compile-e e2 (cons x c))))

;; CEnv -> Integer
(define (next c)
  (- (* 8 (add1 (length c)))))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (symbol=? x y)
       [#t (- (* 8 (length cenv)))]
       [#f (lookup x rest)])]))

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
       