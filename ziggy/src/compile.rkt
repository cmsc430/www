#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1 I J K}
(provide (all-defined-out))
(require "ast.rkt")
{:> B}   (require "compile-ops.rkt")
{:> D0 } (require "types.rkt")
(require a86/ast)

(define rax 'rax)
{:> H0} (define rbx 'rbx) {:> H0} ; heap
{:> E0} (define rsp 'rsp) {:> E0} ; stack
{:> H0} (define rdi 'rdi) {:> H0} ; arg
{:> J}  (define r8  'r8)  {:> J}  ; scratch
{:> F}  (define r15 'r15) {:> F}  ; stack pad (non-volatile)

{:> A I} ;; Expr -> Asm
{:> A I}
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

{:> I} ;; Prog -> Asm
{:> I}
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (Global 'entry)
           (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register
           (Push r15)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '() {:> J} #f)
           (Pop r15)     ; restore callee-save register
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'err)
           pad-stack
           (Call 'raise_error))]))

{:> I} ;; [Listof Defn] -> Asm
{:> I}
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

{:> I} ;; Defn -> Asm
{:> I}
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs) {:> J} #t)
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

{:> F} ;; type CEnv = (Listof [Maybe Id])

{:> A F} ;; Expr -> Asm
{:> F J} ;; Expr CEnv -> Asm
{:> J}   ;; Expr CEnv Boolean -> Asm
(define (compile-e e {:> F} c {:> J} t?)
  (match e
    {:> A D0}
    [(Lit i) (seq (Mov rax i))]
    {:> D0}
    [(Lit d)         (compile-value d)]
    {:> E0}
    [(Eof)           (compile-value eof)]
    {:> H0}
    [(Empty)         (compile-value '())]
    {:> F}
    [(Var x)         (compile-variable x c)]    
    {:> E0}
    [(Prim0 p)       (compile-prim0 p)]
    {:> B}
    [(Prim1 p e)     (compile-prim1 p e {:> F} c)]
    {:> F}
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    {:> H1}
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    {:> C D0}
    [(IfZero e1 e2 e3)
     (compile-ifzero e1 e2 e3 {:> J} t?)]
    {:> D0}
    [(If e1 e2 e3)
     (compile-if e1 e2 e3 {:> F} c {:> J} t?)]
    {:> E0}
    [(Begin e1 e2)
     (compile-begin e1 e2 {:> F} c {:> J} t?)]
    {:> F}
    [(Let x e1 e2)
     (compile-let x e1 e2 c {:> J} t?)]
    {:> I}
    [(App f es)
     (compile-app f es c {:> J} t?)]
    {:> K}
    [(Match e ps es) (compile-match e ps es c t?)]))

{:> D0} ;; Value -> Asm
{:> D0}
(define (compile-value v)
  {:> D0 H1}
  (seq (Mov rax (value->bits v)))
  {:> H1}
  (cond [(string? v) (compile-string v)]
        [else        (Mov rax (value->bits v))]))

{:> F} ;; Id CEnv -> Asm
{:> F}
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

{:> H1} ;; String -> Asm
{:> H1}
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

{:> H1} ;; [Listof Char] Integer -> Asm
{:> H1}
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

{:> E0} ;; Op0 -> Asm
{:> E0}
(define (compile-prim0 p)
  (compile-op0 p))

{:> B F} ;; Op1 Expr -> Asm
{:> F}   ;; Op1 Expr CEnv -> Asm
{:> B}
(define (compile-prim1 p e {:> F} c)
  (seq (compile-e e {:> F} c {:> J} #f)
       (compile-op1 p)))

{:> F} ;; Op2 Expr Expr CEnv -> Asm
{:> F}
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c {:> J} #f)
       (Push rax)
       (compile-e e2 (cons #f c) {:> J} #f)
       (compile-op2 p)))

{:> H1} ;; Op3 Expr Expr Expr CEnv -> Asm
{:> H1}
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c {:> J} #f)
       (Push rax)
       (compile-e e2 (cons #f c) {:> J} #f)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)) {:> J} #f)
       (compile-op3 p)))


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
{:> F J}  ;; Expr Expr Expr CEnv -> Asm
{:> J}    ;; Expr Expr Expr CEnv Boolean -> Asm
{:> D0}
(define (compile-if e1 e2 e3 {:> F} c {:> J} t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 {:> F} c {:> J} #f)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 {:> F} c {:> J} t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 {:> F} c {:> J} t?)
         (Label l2))))

{:> E0 F} ;; Expr Expr -> Asm
{:> F J}  ;; Expr Expr CEnv -> Asm
{:> J}    ;; Expr Expr CEnv Boolean -> Asm
{:> E0}
(define (compile-begin e1 e2 {:> F} c {:> J} t?)
  (seq (compile-e e1 {:> F} c {:> J} #f)
       (compile-e e2 {:> F} c {:> J} t?)))

{:> F J} ;; Id Expr Expr CEnv -> Asm
{:> J}   ;; Id Expr Expr CEnv -> Asm
{:> F}
(define (compile-let x e1 e2 c {:> J} t?)
  (seq (compile-e e1 c {:> J} t?)
       (Push rax)
       (compile-e e2 (cons x c) {:> J} t?)
       (Add rsp 8)))

{:> J} ;; Id [Listof Expr] CEnv Boolean -> Asm
{:> J}
(define (compile-app f es c t?)
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

{:> J} ;; Id [Listof Expr] CEnv -> Asm
{:> J}
(define (compile-app-tail f es c)
  (seq (compile-es es c)
       (move-args (length es) (length c))
       (Add rsp (* 8 (length c)))
       (Jmp (symbol->label f))))

{:> J} ;; Integer Integer -> Asm
{:> J}
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

{:> I} ;; Id [Listof Expr] CEnv -> Asm
{:> I} ;; The return address is placed above the arguments, so callee pops
{:> I} ;; arguments and return address is next frame
{:> I J}
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Jmp (symbol->label f))
         (Label r))))

{:> Z:FIXME} ;; eats previous paren if we do ({:> I J} compile-app {:> J} compile-app-nontail ...)
{:> J}
(define (compile-app-nontail f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Jmp (symbol->label f))
         (Label r))))

{:> I} ;; [Listof Expr] CEnv -> Asm
{:> I}
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c {:> J} #f)
          (Push rax)
          (compile-es es (cons #f c)))]))

{:> K} ;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
{:> K}
(define (compile-match e ps es c t?)
  (let ((done (gensym)))  
    (seq (compile-e e c #f)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'err)
         (Label done)
         (Add rsp 8)))) {:> K} ; pop the saved value being matched

{:> K} ;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
{:> K}
(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))

{:> K} ;; Pat Expr CEnv Symbol Bool -> Asm
{:> K}
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            (Label next))])))

{:> K} ;; Pat CEnv Symbol -> (list Asm CEnv)
{:> K}
(define (compile-pattern p cm next)
  (match p
    [(Var '_)
     (list (seq) cm)]
    [(Var x)
     (list (seq (Push rax)) (cons x cm))]
    [(Lit l)
     (let ((ok (gensym)))
       (list (seq (Cmp rax (value->bits l))
                  (Je ok)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next)
                  (Label ok))
             cm))]
    [(Conj p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            cm2)])])]
    [(Box p)
     (match (compile-pattern p cm next)
       [(list i1 cm1)
        (let ((ok (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Je ok)
                (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                (Jmp next)
                (Label ok)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           cm1))])]
    [(Cons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 cm2)
           (let ((ok (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Je ok)
                   (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                   (Jmp next)
                   (Label ok)
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              cm2))])])]))

{:> F} ;; Id CEnv -> Integer
{:> F}
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
