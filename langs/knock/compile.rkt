#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; An immediate is anything ending in #b000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)
(define type-proc        #b100) ;; <-- NEW: procedure value

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

;; Prog -> Asm
(define (compile p)
  (match p
    [(prog defs e)
     (let ((ds (compile-defines defs))
           (c0 (compile-entry e)))
       `(,@c0
         ,@ds))]))

;; Expr -> Asm
;; Compile e as the entry point
(define (compile-entry e)
  `(entry
    ,@(compile-tail-e e '())
    ret
    err
    (push rbp)
    (call error)
    ret))

;; Expr CEnv -> Asm
;; Compile an expression in tail position
(define (compile-tail-e e c)
  (match e
    [(var-e v)               (compile-variable v c)]
    [(? imm? i)              (compile-imm i)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-tail-if p t f c)]
    [(let-e (list b) body)   (compile-tail-let b body c)]
    [(fun-e f)               (compile-fun f)]
    [(call-e f es)           (compile-fun-tail-call f es c)]
    [(app-e f es)            (compile-tail-call f es c)]))

;; Expr CEnv -> Asm
;; Compile an expression in non-tail position
(define (compile-e e c)
  (match e
    [(var-e v)               (compile-variable v c)]
    [(? imm? i)              (compile-imm i)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-if p t f c)]
    [(let-e (list b) body)   (compile-let b body c)]
    [(fun-e f)               (compile-fun f)]
    [(call-e f es)           (compile-fun-call f es c)]
    [(app-e f es)            (compile-call f es c)]))

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

;; Variable (Listof Expr) CEnv -> Asm
;; Statically know the function we're calling
(define (compile-call f es c)
  (let ((cs (compile-es es (cons #f c)))
        (stack-size (* 8 (length c))))
    `(,@cs
      (sub rsp ,stack-size)
      (call ,(symbol->label f))
      (add rsp ,stack-size))))

;; Variable (Listof Expr) CEnv -> Asm
;; Compile a call in tail position
(define (compile-tail-call f es c)
  (let ((cs (compile-es es c)))
    `(,@cs
      ,@(move-args (length es) (- (length c)))
      (jmp ,(symbol->label f)))))

;; Integer Integer -> Asm
;; Move i arguments upward on stack by offset off
(define (move-args i off)
  (match i
        [0 '()]
        [_ `(,@(move-args (sub1 i) off)
             (mov rbx (offset rsp ,(- off i)))
             (mov (offset rsp ,(- i)) rbx))]))

;; Variable -> Asm
(define (compile-fun f)
  `(; rax <- address of label f
    (lea rax (offset ,(symbol->label f) 0))
    ; write in to heap
    (mov (offset rdi 0) rax)
    ; rax <- pointer into heap
    (mov rax rdi)
    ; tag as procedure pointer
    (or rax ,type-proc)
    ; alloc
    (add rdi 8)))

;; Expr (Listof Expr) CEnv -> Asm
(define (compile-fun-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c))))
        (stack-size (* 8 (length c))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@assert-proc
      (sub rsp ,stack-size)
      (xor rax ,type-proc)
      (call (offset rax 0))
      ;(call ,(symbol->label f))
      (add rsp ,stack-size))))

;; Expr (Listof Expr) CEnv -> Asm
(define (compile-fun-tail-call e0 es c)
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
      (jmp (offset rax 0)))))

;; (Listof Expr) CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c))
           (cs (compile-es es (cons #f c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))

;; Variable (Listof Variable) Expr -> Asm
(define (compile-define def)
  (match def
    [(fundef name args body)
      (let ((c0 (compile-e body (reverse args))))
        `(,(symbol->label name)
          ,@c0
          ret))]))

;; (Listof Variable) (Listof (Listof Variable)) (Listof Expr) -> Asm
(define (compile-defines defs)
  (append-map compile-define defs))

;; Any -> Boolean
(define (imm? x)
  (or (int-e? x)
      (bool-e? x)
      (char-e? x)
      (nil-e? x)))

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

;; Expr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; Expr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
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

;; Expr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 1)))))

;; Expr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 0)))))

;; Expr CEnv -> Asm
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

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
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

;; Expr Expr Expr CEnv -> Asm
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

;; Expr Expr Expr CEnv -> Asm
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

;; Variable Expr Expr CEnv -> Asm
(define (compile-tail-let b e1 c)
  (match b
    [(binding v def)
       (let ((c0 (compile-e def c))
             (c1 (compile-tail-e e1 (cons v c))))
         `(,@c0
           (mov (offset rsp ,(- (add1 (length c)))) rax)
           ,@c1))]
    [_ (error "Compile-let can only handle bindings")]))

;; Variable Expr Expr CEnv -> Asm
(define (compile-let b e1 c)
  (match b
    [(binding v def)
       (let ((c0 (compile-e def c))
             (c1 (compile-e e1 (cons v c))))
         `(,@c0
           (mov (offset rsp ,(- (add1 (length c)))) rax)
           ,@c1))]
    [_ (error "Compile-let can only handle bindings")]))

;; Expr Expr CEnv -> Asm
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
