#lang crook
{:= A B C D0 D1 E0 E1 F H0}
(provide (all-defined-out))
(require "ast.rkt")
{:> B}   (require "compile-ops.rkt")
{:> D0 } (require "types.rkt")
(require a86/ast)

(define rax 'rax)
{:> H0} (define rbx 'rbx) {:> H0} ; heap
{:> E0} (define rsp 'rsp) {:> E0} ; stack
{:> H0} (define rdi 'rdi) ; arg
{:> F}  (define r15 'r15) {:> F}  ; stack pad (non-volatile)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        {:> E0} (Extern 'peek_byte)
        {:> E0} (Extern 'read_byte)
        {:> E0} (Extern 'write_byte)
        {:> E1} (Extern 'raise_error)
        (Label 'entry)
        {:> E0 F} (Sub rsp 8)
        {:> A F}  (compile-e e)
        {:> E0 F} (Add rsp 8)
        {:> F}    (Push r15)    {:> F} ; save callee-saved register
        {:> H0}   (Push rbx)
        {:> H0}   (Mov rbx rdi) {:> H0} ; recv heap pointer
        {:> F}    (compile-e e '())
        {:> H0}   (Pop rbx)
        {:> F}    (Pop r15)     {:> F} ; restore callee-save register
        (Ret)
        {:> E1} ;; Error handler
        {:> E1} (Label 'err)
        {:> F}  pad-stack
        {:> E1} (Call 'raise_error)))

{:> F} ;; type CEnv = (Listof [Maybe Id])

{:> A F} ;; Expr -> Asm
{:> F}   ;; Expr CEnv -> Asm
(define (compile-e e {:> F} c)
  (match e
    {:> A D0} [(Lit i) (seq (Mov rax i))]
    {:> D0}   [(Lit d)         (compile-value d)]
    {:> E0}   [(Eof)           (compile-value eof)]
    {:> H0}   [(Empty)         (compile-value '())]
    {:> F}    [(Var x)         (compile-variable x c)]    
    {:> E0}   [(Prim0 p)       (compile-prim0 p)]
    {:> B}    [(Prim1 p e)     (compile-prim1 p e {:> F} c)]
    {:> F}    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    {:> C D0} [(IfZero e1 e2 e3)
               (compile-ifzero e1 e2 e3)]
    {:> D0}   [(If e1 e2 e3)
               (compile-if e1 e2 e3 {:> F} c)]
    {:> E0}   [(Begin e1 e2)
               (compile-begin e1 e2 {:> F} c)]
    {:> F}    [(Let x e1 e2)
               (compile-let x e1 e2 c)]))

{:> D0} ;; Value -> Asm
{:> D0}
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

{:> F} ;; Id CEnv -> Asm
{:> F}
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

{:> E0} ;; Op0 -> Asm
{:> E0}
(define (compile-prim0 p)
  (compile-op0 p))

{:> B F} ;; Op1 Expr -> Asm
{:> F}   ;; Op1 Expr CEnv -> Asm
{:> B}
(define (compile-prim1 p e {:> F} c)
  (seq (compile-e e {:> F} c)
       (compile-op1 p)))

{:> F} ;; Op2 Expr Expr CEnv -> Asm
{:> F}
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

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

{:> D0 F} ;; Expr Expr Expr -> Asm
{:> F}    ;; Expr Expr Expr CEnv -> Asm
{:> D0}
(define (compile-if e1 e2 e3 {:> F} c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 {:> F} c)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 {:> F} c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 {:> F} c)
         (Label l2))))

{:> E0 F} ;; Expr Expr -> Asm
{:> F}    ;; Expr Expr CEnv -> Asm
{:> E0}
(define (compile-begin e1 e2 {:> F} c)
  (seq (compile-e e1 {:> F} c)
       (compile-e e2 {:> F} c)))

{:> F} ;; Id Expr Expr CEnv -> Asm
{:> F}
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

{:> F} ;; Id CEnv -> Integer
{:> F}
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
