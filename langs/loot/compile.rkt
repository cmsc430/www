#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rcx 'rcx) ; scratch
(define rdx 'rdx) ; return, 2
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type and tail-calls
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile p)
  (match (label-λ (desugar p))                ; <-- changed!
    [(Prog '() e)  
     (prog (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Mov rbx rdi)
           (compile-e e '(#f))
           (Mov rdx rbx)
           (Ret)
           (compile-λ-definitions (λs e)))])) ; <-- changed!

;; [Listof Defn] -> Asm
(define (compile-λ-definitions ds)
  (seq
    (match ds
      ['() (seq)]
      [(cons d ds)
       (seq (compile-λ-definition d)
            (compile-λ-definitions ds))])))

;; This is the code generation for the lambdas themselves.
;; It's not very different from generating code for user-defined functions,
;; because lambdas _are_ user defined functions, they just don't have a name
;;
;; Defn -> Asm
(define (compile-λ-definition l)
  (match l
    [(Lam '() xs e) (error "Lambdas must be labelled before code gen (contact your compiler writer)")]
    [(Lam f xs e)
     (let* ((free (remq* xs (fvs e)))
            ; leave space for RIP
            (env (parity (cons #f (cons #f (reverse (append xs free)))))))
           (seq (Label (symbol->label f))
             (%% "Compiling the body of the function")
             (%% (~a "free vars: " free))
             (%% (~a "args: " xs))
             (%% (~a "env: " env))
             ; we need the #args on the frame, not the length of the entire
             ; env (which may have padding)
             ; Ignore tail calls for now
             (compile-e e env)
             (Ret)))]))

(define (parity c)
  (if (even? (length c))
      (append c (list #f))
      c))

;; Expr Expr Expr CEnv Int -> Asm
(define (compile-tail-e e c s)
  (seq
    (match e
      [(If e1 e2 e3)  (compile-tail-if e1 e2 e3 c s)]
      [(Let x e1 e2)  (compile-tail-let x e1 e2 c s)]
      [(LetRec bs e1) (compile-tail-letrec (map car bs) (map cadr bs) e1 c)]
      [(App f es)     (if (<= (length es) s)
                          (compile-tail-call f es c)
                          (compile-call f es c))]
      [(Begin e1 e2)  (compile-tail-begin e1 e2 c s)]
      [_              (compile-e e c)])))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (seq
       (match e
         [(? imm? i)      (compile-value (get-imm i))]
         [(Var x)         (compile-variable x c)]
         [(App f es)      (compile-call f es c)]
         [(Lam l xs e0)   (compile-λ xs l (fvs e) c)] ; why do we ignore e0?
         [(Prim0 p)       (compile-prim0 p c)]
         [(Prim1 p e)     (compile-prim1 p e c)]
         [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
         [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
         [(Begin e1 e2)   (compile-begin e1 e2 c)]
         [(LetRec bs e1)  (compile-letrec (map car bs) (map cadr bs) e1 c)]
         [(Let x e1 e2)   (compile-let x e1 e2 c)])))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

;; (Listof Variable) Label (Listof Variable) CEnv -> Asm
(define (compile-λ xs f ys c)
  (seq
    ; Save label address
    (Lea rax (symbol->label f))
    (Mov (Offset rbx 0) rax)

    ; Save the environment
    (%% "Begin saving the env")
    (%% (~a "free vars: " ys))
    (Mov r8 (length ys))

    (Mov (Offset rbx 8) r8)
    (Mov r9 rbx)
    (Add r9 16)
    (copy-env-to-heap ys c 0)
    (%% "end saving the env")

    ; Return a pointer to the closure
    (Mov rax rbx)
    (Or rax type-proc)
    (Add rbx (* 8 (+ 2 (length ys))))))

;; (Listof Variable) CEnv Natural -> Asm
;; Pointer to beginning of environment in r9
(define (copy-env-to-heap fvs c i)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq
       ; Move the stack item  in question to a temp register
       (Mov r8 (Offset rsp (lookup x c)))

       ; Put the iterm in the heap
       (Mov (Offset r9 i) r8)

       ; Do it again for the rest of the items, incrementing how
       ; far away from r9 the next item should be
       (copy-env-to-heap fvs c (+ 8 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (match p
         ['add1
          (seq (assert-integer rax)
               (Add rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer rax)
               (Sub rax (imm->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer rax)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['char?
          (let ((l1 (gensym)))
            (seq (And rax mask-char)
                 (Xor rax type-char)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['char->integer
          (seq (assert-char rax)
               (Sar rax char-shift)
               (Sal rax int-shift))]
         ['integer->char
          (seq assert-codepoint
               (Sar rax int-shift)
               (Sal rax char-shift)
               (Xor rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         ['write-byte
          (seq assert-byte
               (pad-stack c)
               (Mov rdi rax)
               (Call 'write_byte)
               (unpad-stack c)
               (Mov rax val-void))]
         ['box
          (seq (Mov (Offset rbx 0) rax)
               (Mov rax rbx)
               (Or rax type-box)
               (Add rbx 8))]
         ['unbox
          (seq (assert-box rax)
               (Xor rax type-box)
               (Mov rax (Offset rax 0)))]
         ['car
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 8)))]
         ['cdr
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 0)))]
         ['empty? (eq-imm val-empty)])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (match p
         ['+
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Add rax r8))]
         ['-
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Sub r8 rax)
               (Mov rax r8))]
         ['eq?
          (let ((l (gensym)))
            (seq (Cmp rax (Offset rsp 0))
                 (Sub rsp 8)
                 (Mov rax val-true)
                 (Je l)
                 (Mov rax val-false)
                 (Label l)))]
         ['cons
          (seq (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16))])))

;; Id [Listof Expr] CEnv -> Asm
;; Here's (part of) why this code is so gross: you have to align the stack for
;; the call but you have to do it *before* evaluating the arguments es,
;; because you need es's values to be just above 'rsp when the call is made.
;; But if you push a frame in order to align the call, you've got to compile es
;; in a static environment that accounts for that frame, hence:
(define (compile-call f es c)
  (let* ((cnt (length es))
         (aligned (even? (+ cnt (length c))))
         (i (if aligned 1 2))
         (c+ (if aligned
                 c
                 (cons #f c)))
         (c++ (cons #f c+)))
    (seq

      (%% "Begin compile-call")
      ; Adjust the stack for alignment, if necessary
      (if aligned
          (seq)
          (Sub rsp 8))

      ; Generate the code for the thing being called
      ; and push the result on the stack
      (compile-e f c+)
      (%% "Push function on stack")
      (Push rax)

      ; Generate the code for the arguments
      ; all results will be put on the stack (compile-es does this)
      (compile-es es c++)
  
      ; Get the function being called off the stack
      ; Ensure it's a proc and remove the tag
      ; Remember it points to the _closure_
      (%% "Get function off stack")
      (Mov rax (Offset rsp (* 8 cnt)))
      (assert-proc rax)
      (Xor rax type-proc)

      (%% "Get closure env")
      (copy-closure-env-to-stack)
      (%% "finish closure env")

      ; get the size of the env and save it on the stack
      (Mov rcx (Offset rax 8))
      (Push rcx)
  
      ; Actually call the function
      (Call (Offset rax 0))
  
      ; Get the size of the env off the stack
      (Pop rcx)
      (Sal rcx 3)

      ; pop args
      ; First the number of arguments + alignment + the closure
      ; then captured values
      (Add rsp (* 8 (+ i cnt)))
      (Add rsp rcx))))


;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-tail-call e0 es c)
  (let ((cnt (length es)))
    (seq
      (%% (~a "Begin compile-tail-call:  function = " e0))
      ; Generate the code for the thing being called
      ; and push the result on the stack
      (compile-e e0 c)
      (%% "Push function on stack")
      (Push rax)

      ; Generate the code for the arguments
      ; all results will be put on the stack (compile-es does this)
      (compile-es es (cons #f c))

      ; Reuse the stack frame (as it's a tail call)
      (move-args cnt (+ cnt (+ 2 (in-frame c))))

      ; Get the function being called off the stack
      ; Ensure it's a proc and remove the tag
      ; Remember it points to the _closure_
      (Mov rax (Offset rsp (* 8 cnt)))
      (assert-proc rax)
      (Xor rax type-proc)

      ; Bump stack pointer (this is where the tail-call
      ; savings kick in)
      (Add rsp (* 8 (+ cnt (+ 2 (in-frame c)))))

      (copy-closure-env-to-stack)

      (Jmp (Offset rax 0)))))

;; -> Asm
;; Copy closure's (in rax) env to stack in rcx
(define (copy-closure-env-to-stack)
  (let ((copy-loop (symbol->label (gensym 'copy_closure)))
        (copy-done (symbol->label (gensym 'copy_done))))
    (seq

      (Mov r8 (Offset rax 8)) ; length
      (Mov r9 rax)
      (Add r9 16)             ; start of env
      (Label copy-loop)
      (Cmp r8 0)
      (Je copy-done)
      (Mov rcx (Offset r9 0))
      (Push rcx)              ; Move val onto stack
      (Sub r8 1)
      (Add r9 8)
      (Jmp copy-loop)
      (Label copy-done))))

;; Integer Integer -> Asm
;; Move i arguments upward on stack by offset off
(define (move-args i cnt)
  (match i
    [0 (seq)]
    [_ (seq
         ; mov first arg to temp reg
         (Mov r9 (Offset rsp (* 8 (sub1 i))))
         ; mov value to correct place on the old frame
         (Mov (Offset rsp (* 8 (+ i cnt))) r9)
         ; Now do the next one
         (move-args (sub1 i) cnt))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr Expr CEnv -> Asm
(define (compile-tail-if e1 e2 e3 c s)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-tail-e e2 c s)
         (Jmp l2)
         (Label l1)
         (compile-tail-e e3 c s)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Expr Expr CEnv -> Asm
(define (compile-tail-begin e1 e2 c s)
  (seq (compile-e e1 c)
       (compile-tail-e e2 c s)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id Expr Expr CEnv -> Asm
(define (compile-tail-let x e1 e2 c s)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-tail-e e2 (cons x c) s)
       (Add rsp 8)))

;; (Listof Variable) (Listof Lambda) Expr CEnv -> Asm
(define (compile-letrec fs ls e c)
  (seq
    (%% (~a  "Start compile letrec with" fs))
    (compile-letrec-λs ls c)
    (compile-letrec-init fs ls (append (reverse fs) c))
    (%% "Finish compile-letrec-init")
    (compile-e e (append (reverse fs) c))
    (Add rsp (* 8 (length fs)))))

;; (Listof Variable) (Listof Lambda) Expr CEnv -> Asm
(define (compile-tail-letrec fs ls e c)
  (seq
    (compile-letrec-λs ls c)
    (compile-letrec-init fs ls (append (reverse fs) c))
    (%% "Finish compile-letrec-init")
    (compile-tail-e e (append (reverse fs) c))
    (Add rsp (* 8 (length fs)))))

;; (Listof Lambda) CEnv -> Asm
;; Create a bunch of uninitialized closures and push them on the stack
(define (compile-letrec-λs ls c)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (match l
       [(Lam lab as body)
        (let ((ys (fvs l)))
             (seq
               (Lea rax (Offset (symbol->label lab) 0))
               (Mov (Offset rbx 0) rax)
               (Mov rax (length ys))
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-proc)
               (Add rbx (* 8 (+ 2 (length ys))))
               (Push rax)
               (compile-letrec-λs ls (cons #f c))))])]))

;; (Listof Variable) (Listof Lambda) CEnv -> Asm
(define (compile-letrec-init fs ls c)
  (match fs
    ['() (seq)]
    [(cons f fs)
     (let ((ys (fvs (first ls))))
          (seq
            (Mov r9 (Offset rsp (lookup f c)))
            (Xor r9 type-proc)
            (Add r9 16) ; move past label and length
            (copy-env-to-heap ys c 0)
            (compile-letrec-init fs (rest ls) c)))]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call with stack arguments
(define (pad-stack-call c i)
  (match (even? (+ (length c) i))
    [#f (seq (Sub rsp 8) (% "padding stack"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (pad-stack-call c 0))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack-call c i)
  (match (even? (+ (length c) i))
    [#f (seq (Add rsp 8) (% "unpadding"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (unpad-stack-call c 0))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error (~a "undefined variable:" x " Env: " cenv))]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (in-frame cenv)
  (match cenv
    ['() 0]
    [(cons #f rest) 0]
    [(cons y rest)  (+ 1 (in-frame rest))]))

(define (assert-type mask type)
  (λ (arg)
    (seq (%% "Begin Assert")
         (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error)
         (%% "End Assert"))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (imm->bits #t))
         (Je l)
         (Mov rax (imm->bits #f))
         (Label l))))
         
(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-proc
  (assert-type ptr-mask type-proc))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (imm->bits 0))
         (Jl 'raise_error)
         (Cmp rax (imm->bits 1114111))
         (Jg 'raise_error)
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'raise_error)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (imm->bits 0))
       (Jl 'raise_error)
       (Cmp rax (imm->bits 255))
       (Jg 'raise_error)))
       
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
