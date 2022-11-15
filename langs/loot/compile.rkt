#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "lambdas.rkt" "fv.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r8 'r8)   ; scratch
(define r9 'r9)   ; scratch
(define r10 'r10) ; scratch
(define r12 'r12) ; reset

;; type CEnv = [Listof Id]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)    ; save callee-saved registers
           (Push r12)
           (Mov rbx rdi) ; recv heap pointer
           (compile-defines-values ds)
           (match (compile-e e (reverse (define-ids ds)) #f)
             [(cons i bs)
              (seq i
                   (Add rsp (* 8 (length ds))) ;; pop function definitions
                   (Pop r12)     ; restore callee-save register
                   (Pop rbx)
                   (Ret)
                   bs)])           
           (compile-defines ds)
           ;(compile-lambda-defines (lambdas p))
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
         (match-let ([(cons is bs) (compile-e e env #t)])              
           (seq (Label (symbol->label f))
                (Mov rax (Offset rsp (* 8 (length xs))))
                (Xor rax type-proc)
                (copy-env-to-stack fvs 8)
                is
                (Add rsp (* 8 (length env))) ; pop env
                (Ret)
                bs)))])))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; Expr CEnv Bool -> (cons Asm Asm)
(define (compile-e e c t?)
  ;(printf "compile-e ~v ~v\n" e c)
  (match e
    [(Int i)            (cons (compile-value i) (seq))]
    [(Bool b)           (cons (compile-value b) (seq))]
    [(Char c)           (cons (compile-value c) (seq))]
    [(Eof)              (cons (compile-value eof) (seq))]
    [(Empty)            (cons (compile-value '()) (seq))]
    [(Var x)            (cons (compile-variable x c) (seq))]
    [(Str s)            (cons (compile-string s) (seq))]
    [(Prim0 p)          (cons (compile-prim0 p c) (seq))]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c t?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c t?)]
    [(Let x e1 e2)      (compile-let x e1 e2 c t?)]
    [(App e es)         (compile-app e es c t?)]
    [(Lam f xs e)       (compile-lam f xs e c)]
    [(Match e ps es)    (compile-match e ps es c t?)]
    [(Reset e)          (compile-reset e c t?)]
    [(Shift x e)        (compile-shift x e c t?)]))

;; Value -> (cons Asm Asm)
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

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

;; Op1 Expr CEnv -> (cons Asm Asm)
(define (compile-prim1 p e c)
  (match (compile-e e c #f)
    [(cons is bs)
     (cons (seq is (compile-op1 p))
           bs)]))

;; Op2 Expr Expr CEnv -> (cons Asm Asm)
(define (compile-prim2 p e1 e2 c)
  (match-let ([(cons is1 bs1) (compile-e e1 c #f)]
              [(cons is2 bs2) (compile-e e2 (cons #f c) #f)])
    (cons (seq is1                   
               (Push rax)
               is2        
               (compile-op2 p))
          (seq bs1 bs2))))

;; Op3 Expr Expr Expr CEnv -> (cons Asm Asm)
(define (compile-prim3 p e1 e2 e3 c)
  (match-let ([(cons is1 bs1) (compile-e e1 c #f)]
              [(cons is2 bs2) (compile-e e2 (cons #f c) #f)]
              [(cons is3 bs3) (compile-e e3 (cons #f (cons #f c)) #f)])
    (cons (seq is1
               (Push rax)
               is2
               (Push rax)
               is3
               (compile-op3 p))
          (seq bs1 bs2 bs3))))

;; Expr Expr Expr CEnv Bool -> (cons Asm Asm)
(define (compile-if e1 e2 e3 c t?)
  (match-let ([(cons is1 bs1) (compile-e e1 c #f)]
              [(cons is2 bs2) (compile-e e2 c t?)]
              [(cons is3 bs3) (compile-e e3 c t?)])
    (cons (let ((l1 (gensym 'if))
                (l2 (gensym 'if)))
            (seq is1
                 (Cmp rax val-false)
                 (Je l1)
                 is2
                 (Jmp l2)
                 (Label l1)
                 is3
                 (Label l2)))
          (seq bs1 bs2 bs3))))

;; Expr Expr CEnv Bool -> (cons Asm Asm)
(define (compile-begin e1 e2 c t?)
  (match-let ([(cons is1 bs1) (compile-e e1 c #f)]
              [(cons is2 bs2) (compile-e e2 c t?)])
    (cons (seq is1 is2)
          (seq bs1 bs2))))

;; Id Expr Expr CEnv Bool -> (cons Asm Asm)
(define (compile-let x e1 e2 c t?)
  (match-let ([(cons is1 bs1) (compile-e e1 c #f)]
              [(cons is2 bs2) (compile-e e2 (cons x c) t?)])
    (cons (seq is1
               (Push rax)
               is2
               (Add rsp 8))
          (seq bs1 bs2))))

;; Id [Listof Expr] CEnv Bool -> (cons Asm Asm)
(define (compile-app f es c t?)
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv -> (cons Asm Asm)
(define (compile-app-tail e es c)
  (match-let ([(cons is bs) (compile-es (cons e es) c)])
    (cons (seq is
               (move-args (add1 (length es)) (length c))
               (Add rsp (* 8 (length c)))
               (Mov rax (Offset rsp (* 8 (length es))))
               (assert-proc rax)
               (Xor rax type-proc)
               (Mov rax (Offset rax 0))
               (Jmp rax))
          bs)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv -> (cons Asm Asm)
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c)
  (match-let ([(cons is bs) (compile-es (cons e es) (cons #f c))])
    (cons (let ((r (gensym 'ret))
                (i (* 8 (length es))))
            (seq (Lea rax r)
                 (Push rax)
                 is
                 (Mov rax (Offset rsp i))
                 (assert-proc rax)
                 (Xor rax type-proc)
                 (Mov rax (Offset rax 0)) ; fetch the code label
                 (Jmp rax)
                 (Label r)))
          bs)))

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

;; Id [Listof Id] Expr CEnv -> (cons Asm Asm)
(define (compile-lam f xs e c)
  ;; Observation: we don't need to traverse e to compute the free variables
  ;; we could instead just filter vars in c (removing duplicates) to get the set
  ;; of free variables.
  (let ((fvs (fv (Lam f xs e))))
    (cons (seq (Lea rax (symbol->label f))
               (Mov (Offset rbx 0) rax)
               (free-vars-to-heap fvs c 8)
               (Mov rax rbx) ; return value
               (Or rax type-proc)
               (Add rbx (* 8 (add1 (length fvs)))))
          
          (let ((env (append (reverse fvs) (reverse xs) (list #f))))
            (match-let ([(cons is bs) (compile-e e env #t)])              
              (seq (Label (symbol->label f))
                   (Mov rax (Offset rsp (* 8 (length xs))))
                   (Xor rax type-proc)
                   (copy-env-to-stack fvs 8)
                   is
                   (Add rsp (* 8 (length env))) ; pop env
                   (Ret)
                   bs))))))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset rbx off) r8)
          (free-vars-to-heap fvs c (+ off 8)))]))

;; [Listof Expr] CEnv -> (cons Asm Asm)
(define (compile-es es c)
  (match es
    ['() (cons '() '())]
    [(cons e es)
     (match-let ([(cons is1 bs1) (compile-e e c #f)]
                 [(cons is2 bs2) (compile-es es (cons #f c))])
       (cons (seq is1
                  (Push rax)
                  is2)
             (seq bs1 bs2)))]))

;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> (cons Asm Asm)
(define (compile-match e ps es c t?)
  (let ((done (gensym)))
    (match-let ([(cons is1 bs1) (compile-e e c #f)]
                [(cons is2 bs2) (compile-match-clauses ps es (cons #f c) done t?)])
      (cons (seq is1
                 (Push rax) ; save away to be restored by each clause
                 is2
                 (Jmp 'raise_error_align)
                 (Label done)
                 (Add rsp 8)) ; pop the saved value being matched
            (seq bs1 bs2)))))

;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> (cons Asm Asm)
(define (compile-match-clauses ps es c done t?)  
  (match* (ps es)
    [('() '()) (cons (seq) (seq))]
    [((cons p ps) (cons e es))
     (match-let ([(cons is1 bs1) (compile-match-clause p e c done t?)]
                 [(cons is2 bs2) (compile-match-clauses ps es c done t?)])
       (cons (seq is1 is2)
             (seq bs1 bs2)))]))

;; Pat Expr CEnv Symbol Bool -> (cons Asm Asm)
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (match-let ([(cons is1 bs1) (compile-e e (append cm c) t?)])
         (cons (seq (Mov rax (Offset rsp 0)) ; restore value being matched
                    i
                    is1
                    (Add rsp (* 8 (length cm)))
                    (Jmp done)
                    f
                    (Label next))
               bs1))])))

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
       (list (seq (Cmp rax (imm->bits l))
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

;; Expr CEnv Boolean -> (cons Asm Asm)
(define (compile-reset e c t?)
  (match-let ([(cons is bs) (compile-e e (cons #f (cons #t c)) #f)])
    (cons (let ((r (gensym 'reset)))
            (seq (Push 'r12)                 
                 (Lea 'r12 r)
                 (Push 'r12)
		 (Mov 'r12 rsp)
                 is
                 (Ret)                 
                 (Label r)
                 (Pop 'r12)))
          bs)))


;; Move things on the stack (possibly) further up
;; overwriting what's there.  You have to be a little
;; careful here because where you're going may overlap
;; with where you are.  So... you have to start from
;; the end, i.e. the highest part of the stack.
;; The whole stack is not being moved up; it's just
;; the variable bindings.

;; Splits an environment into just the variables for
;; a lexical reset and what remains
;; CEnv -> (values CEnv CEnv)
(define (shift-cenv c)
  (match c
    ['() (values '() '())]
    [(cons #f (cons #t _)) (values '() c)]
    [(cons #f c) (shift-cenv c)]
    [(cons x c) (let-values ([(in out) (shift-cenv c)])
                  (values (cons x in) out))]))

;; rax points to the closure
(define (copy-back fv c i)
  (match fv
    ['() (seq)]
    [(cons #t _) (seq)]
    [(cons x fv)
     ;; +16 to skip first two words of the closure
     (seq (Mov r8 (Offset rax (+ 16 (lookup x c))))
          (Mov (Offset rsp i) r8)
          (copy-back fv c (+ i 8)))]))

;; Id Expr CEnv Boolean -> (cons Asm Asm)
(define (compile-shift x e c t?)
  ;; need to be careful about only popping off local-stack
  (let-values ([(c-inside c-outside) (shift-cenv c)]) 
    (let ((fun (gensym 'shift))
          (exit (gensym 'exit)))
      ;; a tiny bit wasteful if x is shadowed
      (match-let ([(cons is bs) (compile-e e (cons x (append c-inside c-outside)) #f)])  
        ;; We're going to reset the stack to some point higher up
        ;; in the address space and then run e.  Since e may depend
        ;; on things in the environment and resetting will destroy
        ;; that environment, we need to save the environment, reset
        ;; the stack, then reinstall the environment.
        ;; It may be tempting to try and do this in-place, but it
        ;; won't work because the reset may be in the middle of c.
        ;; So we copy the values out to the heap and then copy
        ;; them back to the stack.
        (cons (seq (Mov rax rbx)

                   (%% "initialize closure")
                   (Lea r8 fun)
                   (Mov (Offset rbx 0) r8)
                   (Mov r8 r12)
                   (Sub r8 rsp)
                   (Mov (Offset rbx 8) r8)
                   (Add rbx 16)

                   (%% "copy the stack to the heap, reset stack")
                   (let ((loop (gensym 'loop))
                         (done (gensym 'done)))
                     (seq (Label loop)
                          (Cmp r12 rsp)
                          (Je done)
                          (Pop r8)
                          (Mov (Offset rbx 0) r8)
                          (Add rbx 8)
                          (Jmp loop)
                          (Label done)))

                   (%% "alloc local env on stack")
                   (Sub rsp (* (length c-inside) 8))

                   (%% "copy back local env")
                   (copy-back c-inside c 0)

                   (%% "bind to the procedure")
                   (Xor rax type-proc)
                   (Push rax)

                   (%% "run")
                   is

                   (%% "pop off local env")
                   (Add rsp (* 8 (add1 (length c-inside))))

                   (%% "return to the reset")
                   (Ret)

                   (%% "only get here when k is applied")
                   (Label exit))

              (seq (Label fun)
                   (%% "closure and arg on stack")
                   (Pop rax)
                   (Pop r8)
                   (Xor r8 type-proc)
                   (Mov r9 (Offset r8 8))

                   (%% "move to the end of the closure")
                   (Sub r8 16)
                   (Sub r8 r9)

                   (%% "push all the things in the closure")
                   (let ((loop (gensym 'loop))
                         (done (gensym 'done)))
                     (seq (Label loop)
                          (Cmp r9 0)
                          (Je done)
                          (Mov r10 (Offset r8 0))
                          (Push r10)
                          (Add r8 8)
                          (Sub r9 8)
                          (Jmp loop)
                          (Label done)))

                   (%% "jump to the end of the shift")
                   (Jmp exit)
                   bs))))))

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
