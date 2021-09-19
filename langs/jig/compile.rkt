#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type and tail-calls
(define r10  'r10)  ; scratch in assert-type and tail-calls
(define r11  'r11)  ; scratch in assert-type and tail-calls
(define rsp 'rsp) ; stack pointer
(define rbp 'rbp) ; base pointer
(define rdi 'rdi) ; arg

;; type Slot = Variable | Temp | PC | Pad
;; type CEnv = [Listof Slot]

;; Short-hand (we don't mind if all slots of the same kind are the same value)
(struct temp-slot () #:prefab)
(define temp (temp-slot)) 
(struct pc-slot () #:prefab)
(define pc (pc-slot)) 
(struct bp-slot () #:prefab)
(define bp (bp-slot)) 
(struct pad-slot () #:prefab)
(define pad (pad-slot)) 
(struct arg-slot () #:prefab)
(define arg (arg-slot)) 


;; Expr -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (Push rbp)
           (Mov rbp rsp)
           (compile-e e (list bp pc))
           (Mov rdx rbx) ; return heap pointer in second return register           
           (Pop rbp)
           (Ret)
           (compile-defines ds))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (seq
    (match ds
      ['() (seq)]
      [(cons d ds)
       (seq (compile-define d)
            (compile-defines ds))])))
  
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
                        ; leave space for RIP
     (let ((env (parity (cons pc (reverse xs)))))
          (seq (Label (symbol->label f))
               (Push rbp)
               (Mov rbp rsp)
               ;; rsp will, by definition, always be aligned after we push
               ;; rbp on the stack, this is because we are always unaligned
               ;; on entry to a function
               (compile-tail-e e (cons bp env))
               (Mov rsp rbp)
               (Pop rbp)
               (Ret)))]))

(define (parity c)
  (if (even? (length c))
      (append c (list pad))
      c))

;; Expr Expr Expr CEnv -> Asm
(define (compile-tail-e e c)
  (seq
    (match e
      [(If e1 e2 e3) (compile-tail-if e1 e2 e3 c)]
      [(Let x e1 e2) (compile-tail-let x e1 e2 c)]
      [(App f es)    (compile-tail-call f es c)]
      [(Begin e1 e2) (compile-tail-begin e1 e2 c)]
      [_             (compile-e e c)])))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (seq
       (match e
         [(? imm? i)      (compile-value (get-imm i))]
         [(Var x)         (compile-variable x c)]
         [(App f es)      (compile-app f es c)]    
         [(Prim0 p)       (compile-prim0 p c)]
         [(Prim1 p e)     (compile-prim1 p e c)]
         [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
         [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
         [(Begin e1 e2)   (compile-begin e1 e2 c)]
         [(Let x e1 e2)   (compile-let x e1 e2 c)])))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

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
       (compile-e e2 (cons temp c))
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
;; Here's why this code is so gross: you have to align the stack for the call
;; but you have to do it *before* evaluating the arguments es, because you need
;; es's values to be just above 'rsp when the call is made.  But if you push
;; a pad in order to align the call, you've got to compile es in a static
;; environment that accounts for that frame, hence:
(define (compile-app f es c)
  (if (even? (+ (length es) (length c))) 
      (seq (compile-es es c)
           (Call (symbol->label f))
           (Add rsp (* 8 (length es))))            ; pop args
      (seq (Sub rsp 8)                             ; adjust stack
           (compile-es es (cons pad c))
           (Call (symbol->label f))
           (Add rsp (* 8 (add1 (length es)))))))   ; pop args and pad


;; Variable (Listof Expr) CEnv -> Asm
;; Compile a call in tail position
(define (compile-tail-call f es c)
  (let* ((cnt (length es))
         (i (lookup-pc c))
         (align (if (odd? cnt) 8 0)))
        (seq (% (~a "i: " i))
             (% (~a "c: " c))
             (Mov 'r8 (Offset rsp i))
             (Mov 'r11 (Offset rsp (- i 8)))
             (% "Begin compile-es")
             (compile-es es c)         ;; (A)
             (% "End compile-es")
             (Mov r10 (Offset rbp 0))
             ;; We're 16-byte aligned at where BP is (invariant of the CC)
             ;; So for even number args we'll be fine, for odd number args
             ;; we need to pad the stack here
             (Sub r10 align)
             (% "End positioning of move-args pointers")
             (move-args cnt 0)         ;; (B1 + B2)
             (% "End move-args")
             (Mov rsp r10)             ;;
             (Mov rbp r11)             ;;
             (% "End reset rsp and rbp")
             (Push 'r8)                ;;
             (Jmp (symbol->label f)))))


;; (let ((x 42)) (f 84 21))
;;
;; (1)
;;
;; in-frames = 5
;; cnt       = 2
;;
;;                  /-----------------v
;; 0 ... |21|84|42|BP|PC|a3|a2|a1|pd|BP|... MAX_RAM
;; rsp ---^        /                /
;; rbp -----------/                /
;; r10 ---------------------------/
;; r8  = PC 
;; r11 = BP

;; (let ((x 42)) (f 84 21))
;;
;; (2)
;;
;; in-frames = 5
;; cnt       = 2
;;
;;                  /-----------------v
;; 0 ... |21|84|42|BP|PC|a3|a2|a1|pd|BP|... MAX_RAM
;; rsp ---^        /             /
;; rbp -----------/             /
;; r10 ------------------------/
;; r8  = PC 
;; r11 = BP

;; (let ((x 42)) (f 84 21))
;;
;; (A)
;;
;; in-frames = 5
;; cnt       = 2
;;
;;                  /-----------------v
;; 0 ... |21|84|42|BP|PC|a3|a2|a1|pd|BP|... MAX_RAM
;; rsp ---^        /
;; rbp -----------/
;; r8  = PC 
;; r11 = BP

;; (B1)
;;             in-frames #
;;           v--------------v
;; 0 ... |21|84|42|PC|a3|a2|84|pd|... MAX_RAM
;; rsp ---^
;; r8 = PC 

;; (B2)
;;          in-frames #
;;        v--------------v
;; 0 ... |21|84|42|PC|a3|21|84|pd|... MAX_RAM
;; rsp ---^
;; r8 = PC 

;; (C)
;;
;; 0 ... |21|84|42|PC|a3|21|84|pd|... MAX_RAM
;; rsp ------------------^
;;        ^--------------^
;;          in-frames #
;; r8 = PC 

;; (D)
;;
;; 0 ... |21|84|42|PC|PC|21|84|pd|... MAX_RAM
;; rsp ---------------^
;;
;; r8 = PC 


;; Integer -> Asm
;; Move i arguments upward on stack by offset off
(define (move-args i c)
  (match i
    [0 (seq) (Sub r10 (* c 8))]
    [_ (seq
         ; mov first arg to temp reg
         (Mov r9 (Offset rsp (* 8 (sub1 i))))
         ; mov value to correct place on the old frame
         (Mov (Offset r10 (- (* (add1 c) 8))) r9)
         ; Now do the next one
         (move-args (sub1 i) (add1 c)))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons arg c)))]))

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
(define (compile-tail-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-tail-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-tail-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Expr Expr CEnv -> Asm
(define (compile-tail-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-tail-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id Expr Expr CEnv -> Asm
(define (compile-tail-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-tail-e e2 (cons x c))
       (Add rsp 8)))

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
    ['() (error "undefined variable:" x " Env: " cenv)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; CEnv -> Integer
(define (lookup-pc cenv)
  (define (look c)
    (match c
      ['() (error "This should never be the case: No prog-counter found" cenv)]
      [(cons y rest)
       (match (eq? pc y)
         [#t 0]
         [#f (+ 8 (look rest))])]))
  (look cenv))

(define (in-frames cenv)
  (match cenv
    ['() 0]
    [(cons pad rest) 0]
    [(cons y rest)   (+ 1 (in-frames rest))]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error))))

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
