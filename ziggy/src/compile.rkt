#lang crook
{:= A B C D0 D1 E0 E1}
(provide (all-defined-out))
(require "ast.rkt")
{:> B}   (require "compile-ops.rkt")
{:> D0 } (require "types.rkt")
(require a86/ast)

(define rax 'rax)
{:> E0} (define rsp 'rsp) {:> E0} ; stack

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        {:> E0} (Extern 'peek_byte)
        {:> E0} (Extern 'read_byte)
        {:> E0} (Extern 'write_byte)
        {:> E1} (Extern 'raise_error)
        (Label 'entry)
        {:> E0} (Sub rsp 8)
        (compile-e e)
        {:> E0} (Add rsp 8)
        (Ret)
        {:> E1} ;; Error handler
        {:> E1} (Label 'err)
        {:> E1} (Call 'raise_error)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    {:> A D0} [(Lit i) (seq (Mov rax i))]
    {:> D0}   [(Lit d)     (compile-value d)]
    {:> E0}   [(Eof)       (compile-value eof)]
    {:> E0}   [(Prim0 p)   (compile-prim0 p)]
    {:> B}    [(Prim1 p e) (compile-prim1 p e)]
    {:> C D0} [(IfZero e1 e2 e3)
               (compile-ifzero e1 e2 e3)]
    {:> D0}   [(If e1 e2 e3)
               (compile-if e1 e2 e3)]
    {:> E0}   [(Begin e1 e2)
               (compile-begin e1 e2)]))

{:> D0} ;; Value -> Asm
{:> D0}
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

{:> E0} ;; Op0 -> Asm
{:> E0}
(define (compile-prim0 p)
  (compile-op0 p))

{:> B} ;; Op1 Expr -> Asm
{:> B}
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

{:> C D0} ;; Expr Expr Expr -> Asm
{:> C D0}
(define (compile-ifzero e1 e2 e3)
  (let ((l1 (gensym 'ifz))
        (l2 (gensym 'ifz)))
    (seq (compile-e e1)
         (Cmp rax 0)
         (Jne l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

{:> D0} ;; Expr Expr Expr -> Asm
{:> D0}
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

{:> E0} ;; Expr Expr -> Asm
{:> E0}
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))
