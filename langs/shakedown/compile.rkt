#lang racket
(require "syntax.rkt" "ast.rkt")
(provide (all-defined-out))

;; An immediate is anything ending in #b000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)
(define type-proc        #b100)

(define imm-shift        (+ 2 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b00 result-shift))
(define imm-type-bool    (arithmetic-shift #b01 result-shift))
(define imm-type-char    (arithmetic-shift #b10 result-shift))
(define imm-type-empty   (arithmetic-shift #b11 result-shift))
(define imm-val-false    imm-type-bool)
(define imm-val-true
  (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type Imm = Integer | Boolean | Char | ''()

;; type LExpr =
;; ....
;; | `(λ ,Formals ,Label ,Expr)

;; type Label = (quote Symbol)

;; Prog -> Asm
(define (compile p)
  ; Remove all of the explicit function definitions
  (match (desugar-prog p)
    [(prog _ e)
      (compile-entry (label-λ e))]))


;; Expr -> Asm
(define (compile-entry e)
    `(,@(make-externs (ffi-calls e))
      (section text)
      entry
      ,@(compile-tail-e e '())
      ret
      ,@(compile-λ-definitions (λs e))
      err
      (push rbp)
      (call error)
      ret))

;; (Listof Symbol) -> Asm
(define (make-externs fs)
  (map (lambda (s) `(extern ,s)) fs))

;; (Listof Lambda) -> Asm
(define (compile-λ-definitions ls)
  (apply append (map compile-λ-definition ls)))

;; Lambda -> Asm
(define (compile-λ-definition l)
  (match l
    [(lam-t f xs e0)
     (let ((c0 (compile-tail-e e0 (reverse (append xs (fvs l))))))
       `(,f
         ,@c0
         ret))]
    [(lam-e _ _) (error "Lambdas need to be labeled before compiling")]))

;; LExpr CEnv -> Asm
;; Compile an expression in tail position
(define (compile-tail-e e c)
  (match e
    [(var-e v)               (compile-variable v c)]
    [(? imm? i)              (compile-imm i)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-tail-if p t f c)]
    [(let-e (list b) body)   (compile-tail-let b body c)]
    [(letr-e bs body)        (compile-tail-letrec (get-vars bs) (get-defs bs) body c)]
    [(app-e f es)            (compile-tail-call f es c)]
    [(lam-t l xs e0)         (compile-λ xs l (fvs e) c)]))



;; LExpr CEnv -> Asm
;; Compile an expression in non-tail position
(define (compile-e e c)
  (match e
    [(var-e v)               (compile-variable v c)]
    [(? imm? i)              (compile-imm i)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-if p t f c)]
    [(let-e (list b) body)   (compile-let b body c)]
    [(letr-e bs body)        (compile-letrec (get-vars bs) (get-defs bs) body c)]
    [(ccall-e f es)          (compile-ccall f es c)]
    [(app-e f es)            (compile-call f es c)]
    [(lam-t l xs e0)         (compile-λ xs l (fvs e) c)]))

;; Our current set of primitive operations require no function calls,
;; so there's no difference between tail and non-tail call positions
(define (compile-prim p es c)
  (match (cons p es)
    [`(box ,e0)            (compile-box e0 c)]
    [`(unbox ,e0)          (compile-unbox e0 c)]
    [`(cons ,e0 ,e1)       (compile-cons e0 e1 c)]
    [`(car ,e0)            (compile-car e0 c)]
    [`(cdr ,e0)            (compile-cdr e0 c)]
    [`(add1 ,e0)           (compile-add1 e0 c)]
    [`(sub1 ,e0)           (compile-sub1 e0 c)]
    [`(zero? ,e0)          (compile-zero? e0 c)]
    [`(empty? ,e0)         (compile-empty? e0 c)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c)]
    [_            (error
                    (format "prim applied to wrong number of args: ~a ~a" p es))]))

;; Label (listof Expr) -> Asm
(define (compile-ccall f es c)
  (let* ((c0 (store-caller-save caller-saves c))
         (c* (car c0))
         (c1 (compile-es-ffi es c* 0))
         (c2 (cdr (load-caller-save caller-saves c)))
         (stack-size (* 8 (length c*))))

       ; We don't actually have to do all caller-save (that's a lot!)
       ; Just the ones that our compiler emits
      `(,@(cdr c0)

        ,@c1
        (mov r15 rsp) ; Using the fact that r15 is callee save

        ; change rsp to reflect the top of the stack
        (sub rsp ,stack-size)

        ; align rsp to safest 16-byte aligned spot
        (and rsp -16)

        ; Actually call the function
        (call ,f)

        ; Restore our stack
        (mov rsp r15)

        ; Put the caller-saved values back
        ,@c2)))

;; The registers that we can use to pass arguments to C functions
;; (in the right order)
;;
(define arg-regs '(rdi rsi rdx rcx r8 r9))
(define callee-saves '(rbp rbx r12 r13 r14 r15))
(define caller-saves '(rcx rdx rdi rsi r8 r9 r10 r11))

; Make sure we store every caller-save register that we care about on the stack.
; This is basiclaly a foldMR, but I need to learn more Racket
(define (store-caller-save rs c)
  (match rs
    ['()         (cons c '())]
    [(cons r rs)
      (match (store-caller-save rs c)
        [(cons d asm)
          (cons (cons #f d)
                (append asm `((mov (offset rsp ,(- (add1 (length d)))) ,r))))])]))

; Same as above but inverse
(define (load-caller-save rs c)
  (match rs
    ['()         (cons c '())]
    [(cons r rs)
      (match (load-caller-save rs c)
        [(cons d asm)
          (cons (cons #f d)
                (append asm `((mov ,r (offset rsp ,(- (add1 (length d))))))))])]))


;; JMCT: I keep 'programming in Haskell in Racket' and I need to stop that...
;; the above is my monadic habits biting me

;; (Listof LExpr) CEnv -> Asm
(define (compile-es-ffi es c i)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c))
           (cs (compile-es-ffi es c (add1 i))))
       `(,@c0
         (sar rax ,imm-shift)
         (mov ,(list-ref arg-regs i) rax) ; Put the result in the appropriate register
         ,@cs))]))


;; (Listof Variable) Label (Listof Variable) CEnv -> Asm
(define (compile-λ xs f ys c)
    ; Save label address
  `((lea rax (offset ,f 0))
    (mov (offset rdi 0) rax)

    ; Save the environment
    (mov r8 ,(length ys))
    (mov (offset rdi 1) r8)
    (mov r9 rdi)
    (add r9 16)
    ,@(copy-env-to-heap ys c 0)

    ; Return a pointer to the closure
    (mov rax rdi)
    (or rax ,type-proc)
    (add rdi ,(* 8 (+ 2 (length ys))))))

;; (Listof Variable) CEnv Natural -> Asm
;; Pointer to beginning of environment in r9
(define (copy-env-to-heap fvs c i)
  (match fvs
    ['() '()]
    [(cons x fvs)
     `((mov r8 (offset rsp ,(- (add1 (lookup x c)))))
       (mov (offset r9 ,i) r8)
       ,@(copy-env-to-heap fvs c (add1 i)))]))

;; Natural Natural -> Asm
;; Move i arguments upward on stack by offset off
(define (move-args i off)
  (match i
        [0 '()]
        [_ `(,@(move-args (sub1 i) off)
             (mov rbx (offset rsp ,(- off i)))
             (mov (offset rsp ,(- i)) rbx))]))

;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c))))
        (stack-size (* 8 (length c))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@assert-proc
      (xor rax ,type-proc)
      (sub rsp ,stack-size)

      (mov rcx rsp) ; start of stack in rcx
      (add rcx ,(- (* 8 (+ 2 (length es)))))
      ,@(copy-closure-env-to-stack)

      (call (offset rax 0))
      (add rsp ,stack-size))))

;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-tail-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c)))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@(move-args (length es) i)
      ,@assert-proc
      (xor rax ,type-proc)

      (mov rcx rsp) ; start of stack in rcx
      (add rcx ,(- (* 8 (+ 1 (length es)))))
      ,@(copy-closure-env-to-stack)

      ;,@(copy-closure-env-to-stack (length es))
      (jmp (offset rax 0)))))


;; -> Asm
;; Copy closure's (in rax) env to stack in rcx
(define (copy-closure-env-to-stack)
  (let ((copy-loop (gensym 'copy_closure))
        (copy-done (gensym 'copy_done)))
    `((mov r8 (offset rax 1)) ; length
      (mov r9 rax)
      (add r9 16)             ; start of env
      ,copy-loop
      (cmp r8 0)
      (je ,copy-done)
      (mov rbx (offset r9 0))
      (mov (offset rcx 0) rbx) ; Move val onto stack
      (sub r8 1)
      (add r9 8)
      (sub rcx 8)
      (jmp ,copy-loop)
      ,copy-done)))

;; (Listof Variable) (Listof Lambda) Expr CEnv -> Asm
(define (compile-letrec fs ls e c)
  (let ((c0 (compile-letrec-λs ls c))
        (c1 (compile-letrec-init fs ls (append (reverse fs) c)))
        (c2 (compile-e e (append (reverse fs) c))))
    `(,@c0
      ,@c1
      ,@c2)))

;; (Listof Variable) (Listof Lambda) Expr CEnv -> Asm
(define (compile-tail-letrec fs ls e c)
  (let ((c0 (compile-letrec-λs ls c))
        (c1 (compile-letrec-init fs ls (append (reverse fs) c)))
        (c2 (compile-tail-e e (append (reverse fs) c))))
    `(,@c0
      ,@c1
      ,@c2)))

;; (Listof Lambda) CEnv -> Asm
;; Create a bunch of uninitialized closures and push them on the stack
(define (compile-letrec-λs ls c)
  (match ls
    ['() '()]
    [(cons l ls)
     (match l
       [(lam-t lab as body)
         (let ((cs (compile-letrec-λs ls (cons #f c)))
               (ys (fvs l)))
           `((lea rax (offset ,lab 0))
             (mov (offset rdi 0) rax)
             (mov rax ,(length ys))
             (mov (offset rdi 1) rax)
             (mov rax rdi)
             (or rax ,type-proc)
             (add rdi ,(* 8 (+ 2 (length ys))))
             (mov (offset rsp ,(- (add1 (length c)))) rax)
             ,@cs))])]))

;; (Listof Variable) (Listof Lambda) CEnv -> Asm
(define (compile-letrec-init fs ls c)
  (match fs
    ['() '()]
    [(cons f fs)
     (let ((ys (fvs (first ls)))
           (cs (compile-letrec-init fs (rest ls) c)))
       `((mov r9 (offset rsp ,(- (add1 (lookup f c)))))
         (xor r9 ,type-proc)
         (add r9 16) ; move past label and length
         ,@(copy-env-to-heap ys c 0)
         ,@cs))]))

;; (Listof LExpr) CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c))
           (cs (compile-es es (cons #f c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

;; Imm -> Integer
(define (imm->bits i)
  (match i
    [(int-e i)  (arithmetic-shift i imm-shift)]
    [(char-e c) (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(bool-e b) (if b imm-val-true imm-val-false)]
    [(nil-e)    imm-type-empty]))


;; Variable CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; LExpr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; LExpr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; LExpr LExpr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 0) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 1) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; LExpr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 1)))))

;; LExpr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 0)))))

;; LExpr CEnv -> Asm
(define (compile-empty? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-empty)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; LExpr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; LExpr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; LExpr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; LExpr LExpr LExpr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

;; LExpr LExpr LExpr CEnv -> Asm
(define (compile-tail-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-tail-e e1 c))
        (c2 (compile-tail-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

;; Variable LExpr LExpr CEnv -> Asm
(define (compile-tail-let b body c)
  (match b
    [(binding x def) 
      (let ((c0 (compile-e def c))
            (c1 (compile-tail-e body (cons x c))))
        `(,@c0
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1))]))

;; Variable LExpr LExpr CEnv -> Asm
(define (compile-let b body c)
  (match b
    [(binding x def) 
      (let ((c0 (compile-e def c))
            (c1 (compile-e body (cons x c))))
        `(,@c0
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1))]))

;; LExpr LExpr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))


(define (type-pred->mask p)
  (match p
    [(or 'box? 'cons? 'string? 'procedure?) result-type-mask]
    [_ imm-type-mask]))

(define (type-pred->tag p)
  (match p
    ['box?       type-box]
    ['cons?      type-pair]
    ['string?    type-string]
    ['procedure? type-proc]
    ['integer?   imm-type-int]
    ['empty?     imm-type-empty]
    ['char?      imm-type-char]
    ['boolean?   imm-type-bool]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'cons?))
(define assert-string  (assert-type 'string?))
(define assert-char    (assert-type 'char?))
(define assert-proc    (assert-type 'procedure?))

;; Asm
(define assert-natural
  `(,@assert-integer
    (cmp rax -1)
    (jle err)))

;; Asm
(define assert-integer-codepoint
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx 0)
    (jne err)
    (cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (cmp rax ,(arithmetic-shift #x10FFFF imm-shift))
    (mov rbx rax)
    (sar rbx ,(+ 11 imm-shift))
    (cmp rbx #b11011)
    (je err)))
