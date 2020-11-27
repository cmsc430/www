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
    [(Int i)         (compile-value i)]
    [(Bool b)        (compile-value b)]
    [(Char c)        (compile-value c)]
    [(Eof)           (compile-value eof)]
    [(Empty)         (compile-value '())]
    [(Var x)         (compile-variable x c)]
    [(Prim0 p)       (compile-prim0 p c)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)   (compile-begin e1 e2 c)]
    [(Let x e1 e2)   (compile-let x e1 e2 c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov 'rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov 'rax (Offset 'rsp i)))))

;; adjust in 16-byte alignments
(define (stack-adjust c)
  (let ((l (length c)))
    (if (even? l)
        (* 8 l)
        (* 8 (add1 l)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['read-byte
     (let ((l (stack-adjust c)))
       (seq (Sub 'rsp l)
            (Push 'rbp)
            (Push 'rsp)
            (Push 'rdi) ; save heap loc
            (Mov 'rdi 'rax)
            (Call 'read_byte)
            (Pop 'rdi)
            (Pop 'rsp)
            (Pop 'rbp)
            (Add 'rsp l)))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (match p
         ['add1
          (seq (assert-integer 'rax)
               (Add 'rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer 'rax)
               (Sub 'rax (imm->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer 'rax)
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
          (seq (assert-char 'rax)
               (Sar 'rax char-shift)
               (Sal 'rax int-shift))]
         ['integer->char
          (seq assert-codepoint
               (Sar 'rax int-shift)
               (Sal 'rax char-shift)
               (Xor 'rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         ['write-byte
          (let ((l (stack-adjust c)))
            (seq assert-byte
                 (Sub 'rsp l)
                 (Push 'rbp)
                 (Push 'rsp)
                 (Push 'rdi) ; save heap loc
                 (Mov 'rdi 'rax)
                 (Call 'write_byte)
                 (Pop 'rdi) ; restore
                 (Pop 'rsp)
                 (Pop 'rbp)
                 (Add 'rsp l)
                 (Mov 'rax val-void)))]
         ['box
          (seq (Mov (Offset 'rdi 0) 'rax)
               (Mov 'rax 'rdi)
               (Or 'rax type-box)
               (Add 'rdi 8))]
         ['unbox
          (seq (assert-box 'rax)
               (Xor 'rax type-box)
               (Mov 'rax (Offset 'rax 0)))]
         ['car
          (seq (assert-cons 'rax)
               (Xor 'rax type-cons)
               (Mov 'rax (Offset 'rax 8)))]
         ['cdr
          (seq (assert-cons 'rax)
               (Xor 'rax type-cons)
               (Mov 'rax (Offset 'rax 0)))]
         ['empty? (eq-imm val-empty)])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (let ((i (next c)))
    (seq (compile-e e1 c)       
         (Mov (Offset 'rsp i) 'rax)
         (compile-e e2 (cons #f c))       
         (match p
           ['+
            (seq (assert-integer (Offset 'rsp i))
                 (assert-integer 'rax)   
                 (Add 'rax (Offset 'rsp i)))]
           ['-
            (seq (assert-integer (Offset 'rsp i))
                 (assert-integer 'rax)   
                 (Sub (Offset 'rsp i) 'rax)
                 (Mov 'rax (Offset 'rsp i)))]
           ['cons
            (seq (Mov (Offset 'rdi 0) 'rax)
                 (Mov 'rax (Offset 'rsp i))
                 (Mov (Offset 'rdi 8) 'rax)
                 (Mov 'rax 'rdi)
                 (Or 'rax type-cons)
                 (Add 'rdi 16))]))))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp 'rax imm)
         (Mov 'rax val-true)
         (Je l1)
         (Mov 'rax val-false)
         (Label l1))))

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
     (match (eq? x y)
       [#t (- (* 8 (length cenv)))]
       [#f (lookup x rest)])]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov 'rbx arg)
         (And 'rbx mask)
         (Cmp 'rbx type)
         (Jne 'err))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq (assert-integer 'rax)
         (Cmp 'rax (imm->bits 0))
         (Jl 'err)
         (Cmp 'rax (imm->bits 1114111))
         (Jg 'err)
         (Cmp 'rax (imm->bits 55295))
         (Jl ok)
         (Cmp 'rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer 'rax)
       (Cmp 'rax (imm->bits 0))
       (Jl 'err)
       (Cmp 'rax (imm->bits 255))
       (Jg 'err)))
       