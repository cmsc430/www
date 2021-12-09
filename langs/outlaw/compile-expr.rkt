#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "lambdas.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-ops.rkt"
         "compile-datum.rkt"
         a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; Expr CEnv GEnv Bool -> Asm
(define (compile-e e c g t?)
  (match e
    [(Quote d)          (compile-datum d)]
    [(Eof)              (seq (Mov rax (imm->bits eof)))]
    [(Var x)            (compile-variable x c g)]
    [(Prim p es)        (compile-prim p es c g)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c g t?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c g t?)]
    [(Let xs es e)      (compile-let xs es e c g t?)]
    [(App e es)         (compile-app e es c g t?)]
    [(Apply e es el)    (compile-apply e es el c g t?)]
    [(Lam _ _ _)        (compile-lam e c g)]
    [(LamRest _ _ _ _)  (compile-lam e c g)]
    [(LamCase _ _)      (compile-lam e c g)]
    [(Match e ps es)    (compile-match e ps es c g t?)]))

;; Id CEnv GEnv -> Asm
(define (compile-variable x c g)
  (match (lookup x c)
    [#f (if (memq x g)
            (seq (Mov rax (Offset (symbol->label x) 0)))
            (error "unbound variable" x))]
    [i  (seq (Mov rax (Offset rsp i)))]))

;; Op (Listof Expr) CEnv GEnv -> Asm
(define (compile-prim p es c g)
  (seq (compile-es* es c g)
       (match p
         ['make-struct (compile-make-struct (length es))]
         [_ (compile-op p)])))

;; Expr Expr Expr CEnv GEnv Bool -> Asm
(define (compile-if e1 e2 e3 c g t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c g #f)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c g t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c g t?)
         (Label l2))))

;; Expr Expr CEnv GEnv Bool -> Asm
(define (compile-begin e1 e2 c g t?)
  (seq (compile-e e1 c g #f)
       (compile-e e2 c g t?)))

;; [Listof Id] [Listof Expr] Expr CEnv GEnv Bool -> Asm
(define (compile-let xs es e c g t?)
  (seq (compile-es es c g)
       (compile-e e (append (reverse xs) c) g t?)
       (Add rsp (*8 (length xs)))))

;; Id [Listof Expr] CEnv GEnv Bool -> Asm
(define (compile-app f es c g t?)
  (compile-app-nontail f es c g)
  #;
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv GEnv -> Asm
(define (compile-app-tail e es c g)
  (seq (compile-es (cons e es) c g)
       (move-args (add1 (length es)) (length c))
       (Add rsp (*8 (length c)))
       (Mov rax (Offset rsp (*8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (*8 (sub1 i))))
              (Mov (Offset rsp (*8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv GEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c g)
  (let ((r (gensym 'ret))
        (i (*8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c) g)
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov r15 (length es))
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))

;; Expr [Listof Expr] Expr CEnv GEnv Boolean -> Asm
(define (compile-apply e es el c g t?)
  ;; FIXME: should have tail recursive version too
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c) g)
         (compile-e el (append (make-list (add1 (length es)) #f) (cons #f c)) g #f)

         (Mov r10 (Offset rsp (*8 (length es))))
         
         (Mov r15 (length es))
         (let ((loop (gensym))
               (done (gensym)))
           (seq (Label loop)
                (Cmp rax val-empty)
                (Je done)
                (assert-cons rax)
                (Add r15 1)
                (Xor rax type-cons)
                (Mov r9 (Offset rax 8))
                (Push r9)
                (Mov rax (Offset rax 0))
                (Jmp loop)
                (Label done)))


         (assert-proc r10)
         (Xor r10 type-proc)
         (Mov r10 (Offset r10 0))
         
         (Jmp r10)
         (Label r))))

;; Lambda CEnv GEnv -> Asm
(define (compile-lam l c g)
  (let ((fvs (fv- l g)))
    (seq (Lea rax (symbol->label (lambda-name l)))
         (Mov (Offset rbx 0) rax)
         (free-vars-to-heap fvs c 8)
         (Mov rax rbx) ; return value
         (Or rax type-proc)         
         (Add rbx (*8 (add1 (length fvs)))))))

;; Lambda -> Id
(define (lambda-name l)
  (match l
    [(Lam f _ _) f]
    [(LamRest f _ _ _) f]
    [(LamCase f _) f]))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (match (lookup x c)
       [#f (error "unbound variable" x)]
       [i
        (seq (Mov r8 (Offset rsp i))
             (Mov (Offset rbx off) r8)
             (free-vars-to-heap fvs c (+ off 8)))])]))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; [Listof Expr] CEnv GEnv -> Asm
(define (compile-es es c g)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c g #f)
          (Push rax)
          (compile-es es (cons #f c) g))]))

;; [Listof Expr] CEnv GEnv -> Asm
;; Like compile-es, but leave last subexpression in rax (if exists)
(define (compile-es* es c g)
  (match es
    ['() '()]
    [(cons e '())
     (compile-e e c g #f)]
    [(cons e es)
     (seq (compile-e e c g #f)
          (Push rax)
          (compile-es* es (cons #f c) g))]))

;; Expr [Listof Pat] [Listof Expr] CEnv GEnv Bool -> Asm
(define (compile-match e ps es c g t?)
  (let ((done (gensym)))
    (seq (compile-e e c g #f)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) g done t?)
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv GEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c g done t?)
  (match (cons ps es)
    [(cons '() '()) (seq)]
    [(cons (cons p ps) (cons e es))
     (seq (compile-match-clause p e c g done t?)
          (compile-match-clauses ps es c g done t?))]))

;; Pat Expr CEnv GEnv Symbol Bool -> Asm
(define (compile-match-clause p e c g done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) g t?)
            (Add rsp (*8 (length cm)))
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
    [(PStr s)
     (let ((fail (gensym)))
       (list (seq (Lea rdi (symbol->data-label (string->symbol s)))
                  (Mov r8 rax)
                  (And r8 ptr-mask)
                  (Cmp r8 type-str)
                  (Jne fail)
                  (Xor rax type-str)
                  (Mov rsi rax)
                  pad-stack
                  (Call 'symb_cmp)
                  unpad-stack
                  (Cmp rax 0)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
                  (Jmp next))
             cm))]
    [(PSymb s)
     (let ((fail (gensym)))
       (list (seq (Lea r9 (Plus (symbol->data-label s) type-symb))
                  (Cmp rax r9)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
                  (Jmp next))
             cm))]
    [(PLit l)
     (let ((fail (gensym)))
       (list (seq (Cmp rax (imm->bits l))
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
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
                 (Mov rax (Offset rsp (*8 (- (sub1 (length cm1)) (length cm)))))
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
                (Add rsp (*8 (length cm))) ; haven't pushed anything yet
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
                   (Mov rax (Offset rsp (*8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              (seq f1
                   f2
                   (Label fail)
                   (Add rsp (*8 (length cm))) ; haven't pushed anything yet
                   (Jmp next))
              cm2))])])]
    [(PStruct n ps)
     (match (compile-struct-patterns ps (cons #f cm) next 1)
       [(list i f cm)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-struct)
                (Jne fail)
                (Xor rax type-struct)
                (Mov r8 (Offset rax 0))
                (Lea r9 (Plus (symbol->data-label n) type-symb))
                (Cmp r8 r9)
                (Jne fail)
                (Push rax)
                i)
           (seq f
                (Label fail)
                (Add rsp (*8 (length cm)))
                (Jmp next))
           cm))])]))

;; [Listof Pat] CEnv Symbol Nat -> (list Asm Asm CEnv)
(define (compile-struct-patterns ps cm next i)
  (match ps
    ['() (list '() '() cm)]
    [(cons p ps)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (match (compile-struct-patterns ps cm1 next (add1 i))
          [(list is fs cmn)
           (list
            (seq (Mov rax (Offset rax (*8 i)))
                 i1
                 (Mov rax (Offset rsp (*8 (sub1 (length cm1)))))
                 is)
            (seq f1 fs)
            cmn)])])]))
