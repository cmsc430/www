#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "lambdas.rkt" "fv.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r15 'r15) ; stack pad (non-volatile)

;; type CEnv = [Listof Id]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register
           (Push r15)        
           (Mov rbx rdi) ; recv heap pointer
           (compile-defines-values ds)
           (compile-e e (reverse (define-ids ds)) #f)
           (Add rsp (* 8 (length ds))) ;; pop function definitions
           (Pop r15)     ; restore callee-save register
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (compile-lambda-defines (lambdas p))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (cons f (define-ids ds))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (compile-lambda-define (Lam f xs e))]))

;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))

;; Lam -> Asm
(define (compile-lambda-define l)
  (let ((fvs (fv l)))
    (match l
      [(Lam f xs e)
       (let ((env  (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Label (symbol->label f))              
              (Mov rax (Offset rsp (* 8 (length xs))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)
              (compile-e e env #t)
              (Add rsp (* 8 (length env))) ; pop env
              (Ret)))])))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; Expr CEnv Bool -> Asm
(define (compile-e e c t?)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c t?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c t?)]
    [(Let x e1 e2)      (compile-let x e1 e2 c t?)]
    [(App e es)         (compile-app e es c t?)]
    [(Lam f xs e)       (compile-lam f xs e c)]
    [(Match e ps es)    (compile-match e ps es c t?)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
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

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c #f)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)) #f)
       (compile-op3 p)))

;; Expr Expr Expr CEnv Bool -> Asm
(define (compile-if e1 e2 e3 c t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c #f)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c t?)
         (Label l2))))

;; Expr Expr CEnv Bool -> Asm
(define (compile-begin e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (compile-e e2 c t?)))

;; Id Expr Expr CEnv Bool -> Asm
(define (compile-let x e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons x c) t?)
       (Add rsp 8)))

;; Id [Listof Expr] CEnv Bool -> Asm
(define (compile-app f es c t?)
  ;(compile-app-nontail f es c)
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv -> Asm
(define (compile-app-tail e es c)
  (seq (compile-es (cons e es) c)
       (move-args (add1 (length es)) (length c))
       (Add rsp (* 8 (length c)))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c)
  (let ((r (gensym 'ret))
        (i (* 8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c))         
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))

;; Defns -> Asm
;; Compile the closures for ds and push them on the stack
(define (compile-defines-values ds)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds)) 8)
       (add-rbx-defines ds 0)))

;; Defns Int -> Asm
;; Allocate closures for ds at given offset, but don't write environment yet
(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx off) rax)         
            (Mov rax rbx)
            (Add rax off)
            (Or rax type-proc)
            (Push rax)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns CEnv Int -> Asm
;; Initialize the environment for each closure for ds at given offset
(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns Int -> Asm
;; Compute adjustment to rbx for allocation of all ds
(define (add-rbx-defines ds n)
  (match ds
    ['() (seq (Add rbx (* n 8)))]
    [(cons (Defn f xs e) ds)
     (add-rbx-defines ds (+ n (add1 (length (fv (Lam f xs e))))))]))

;; Id [Listof Id] Expr CEnv -> Asm
(define (compile-lam f xs e c) 
  (let ((fvs (fv (Lam f xs e))))
    (seq (Lea rax (symbol->label f))
         (Mov (Offset rbx 0) rax)
         (free-vars-to-heap fvs c 8)
         (Mov rax rbx) ; return value
         (Or rax type-proc)         
         (Add rbx (* 8 (add1 (length fvs)))))))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset rbx off) r8)
          (free-vars-to-heap fvs c (+ off 8)))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c #f)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
(define (compile-match e ps es c t?)
  (let ((done (gensym)))
    (seq (compile-e e c #f)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))

;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            f
            (Label next))])))

;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    [(PWild)
     (list (seq) (seq) cm)]
    [(PVar x)
     (list (seq (Push rax))
           (seq)
           (cons x cm))]
    [(PLit l)
     (let ((fail (gensym)))
       (list (seq (Cmp rax (value->bits l))
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next))
             cm))]
    [(PAnd p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            (seq f1 f2)
            cm2)])])]
    [(PBox p)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Jne fail)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           (seq f1
                (Label fail)
                (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                (Jmp next))
           cm1))])]
    [(PCons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (let ((fail (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Jne fail)
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              (seq f1
                   f2
                   (Label fail)
                   (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                   (Jmp next))
              cm2))])])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
