#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "lambdas.rkt" "fv.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '() #t)
           (Ret)
           (compile-lambda-defines (lambdas e))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))

;; Lam -> Asm
(define (compile-lambda-define l)
  (match l
    [(Lam f xs e)
     (let ((env (append (fv l) (reverse xs))))
       (seq (Label (symbol->label f))
            (compile-e e env #t)
            (Add rsp (* 8 (length env))) ; pop env
            (Ret)))]))

;; Defn -> Asm
#;
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs) #t)
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

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
    [(Lam f xs e)       (compile-lam f xs e c)]))

;; Value -> Asm
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
  ;; FIXME: turn off tail calls for now
  (compile-app-nontail f es c)  
  #;(if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv -> Asm
(define (compile-app-tail f es c)
  (seq (compile-es es c)
       (move-args (length es) (length c))
       (Add rsp (* 8 (length c)))
       (Jmp (symbol->label f))))

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
    (seq (compile-e e c #f)
         (Push rax)
         (compile-es es (cons #f c))

         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov r8 (Offset rax 0))

         (Lea r10 r)
         (Mov (Offset rsp i) r10)
         
         ; copy-heap-to-stack
         (Mov r9 (Offset rax 8))
         (Add rax 16)
         (let ((loop (gensym))
               (done (gensym)))
           (seq (Label loop)
                (Cmp r9 0)
                (Je done)
                (Mov r10 (Offset rax 0))
                (Push r10)
                (Add rax 8)
                (Sub r9 1)
                (Jmp loop)
                (Label done)))
         
         (Jmp r8)
         (Label r))))



;; Id [Listof Id] Expr CEnv -> Asm
(define (compile-lam f xs e c)
  (let ((fvs (fv (Lam f xs e))))
    (seq (Lea rax (symbol->label f))
         (Mov (Offset rbx 0) rax)
         (Mov rax (length fvs))
         (Mov (Offset rbx 8) rax)

         (Mov rax rbx)      ; return value
         (Or rax type-proc)
         (Add rbx 16)
         ;; copy values of free vars to heap
         (free-vars-to-heap fvs c)
         )))

(define (free-vars-to-heap fvs c)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq 
      (Mov r8 (Offset rsp (lookup x c)))
      (Mov (Offset rbx 0) r8)
      (Add rbx 8)
      (free-vars-to-heap fvs c))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c #f)
          (Push rax)
          (compile-es es (cons #f c)))]))

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
