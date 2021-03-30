#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guards

;; These are used to guard the instruction constructors to reject bad inputs
;; with decent error messages.

(define check:label-symbol
  (λ (x n)
    (when (register? x)
      (error n "cannot use register as label name; given ~v" x))
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    x))

(define check:target
  (λ (x n)
    (unless (or (symbol? x) (offset? x)); either register or label
      (error n "expects symbol; given ~v" x))
    x))

(define check:arith  
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (exact-integer? a2) (register? a2) (offset? a2))
      (error n "expects exact integer, register, or offset; given ~v" a2))
    (values a1 a2)))

(define check:register
  (λ (a1 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    a1))

(define check:src-dest
  (λ (a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2))
      (error n "expects register, offset, or exact integer; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (values a1 a2)))

(define check:shift
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (and (exact-integer? a2) (<= 0 a2 63))
      (error n "expects exact integer in [0,63]; given ~v" a2))
    (values a1 a2)))      

(define check:offset
  (λ (r i n)
    (unless (register? r)
      (error n "expects register as first argument; given ~v" r))
    (unless (exact-integer? i)
      (error n "expects exact integer as second argument; given ~v" i))
    (values r i)))

(define check:push
  (λ (a1 n)
    (unless (or (exact-integer? a1) (register? a1))
      (error n "expects exact integer or register; given ~v" a1))
    a1))

(define check:lea
  (λ (dst x n)
    (unless (or (register? dst) (offset? dst))
      (error n "expects register or offset; given ~v" dst))
    (unless (label? x)
      (error n "expects label; given ~v" x))
    (values dst x)))

(define check:none
  (λ (n) (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(provide (struct-out %)
         (struct-out %%)
         (struct-out %%%)
         Comment?)

(struct Comment (str)
  #:transparent
  #:guard
  (λ (s n)
    (unless (string? s)
      (error n "expects string; given ~v" s))
    s))

(struct %   Comment () #:transparent)
(struct %%  Comment () #:transparent)
(struct %%% Comment () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions

(define-syntax-rule
  (instruct Name (x ...) guard)
  (begin (provide (struct-out Name))
         (struct Name (x ...)
           #:transparent
           #:guard guard)))

(instruct Label  (x)       check:label-symbol)
(instruct Call   (x)       check:target)
(instruct Ret    ()        check:none)
(instruct Mov    (dst src) check:src-dest)
(instruct Add    (dst src) check:arith)
(instruct Sub    (dst src) check:arith)
(instruct Cmp    (a1 a2)   check:src-dest)
(instruct Jmp    (x)       check:target)
(instruct Je     (x)       check:target)
(instruct Jne    (x)       check:target)
(instruct Jl     (x)       check:target)
(instruct Jg     (x)       check:target)
(instruct And    (dst src) check:src-dest)
(instruct Or     (dst src) check:src-dest)
(instruct Xor    (dst src) check:src-dest)
(instruct Sal    (dst i)   check:shift)
(instruct Sar    (dst i)   check:shift)
(instruct Push   (a1)      check:push)
(instruct Pop    (a1)      check:register)
(instruct Lea    (dst x)   check:lea)

(instruct Offset (r i)     check:offset)
(instruct Extern (x)       check:label-symbol)

(provide offset? register? instruction? label?)

(define offset? Offset?)

(define (register? x)
  (and (memq x '(rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
       #t))

(define (label? x)
  (and (symbol? x)
       (not (register? x))))

(define (instruction? x)
  (or (Label? x)
      (Extern? x)
      (Call? x)
      (Ret? x)
      (Mov? x)
      (Add? x)
      (Sub? x)
      (Cmp? x)
      (Jmp? x)
      (Je? x)
      (Jne? x)
      (Jl? x)
      (Jg? x)
      (And? x)
      (Or? x)
      (Xor? x)
      (Sal? x)
      (Sar? x)
      (Push? x)
      (Pop? x)
      (Lea? x)
      (Comment? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction sequencing and program error checking

(provide/contract
 [seq   (-> (or/c instruction? (listof instruction?)) ...
            (listof instruction?))]
 [prog (-> (or/c instruction? (listof instruction?)) ...
           (listof instruction?))])

;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))

;; (U Instruction Asm) ... -> Asm
;; Construct a "program", does some global well-formedness checking to help
;; prevent confusing error messages as the nasm level
(define (prog . xs)
  (let ((p (apply seq xs)))
    (check-unique-label-decls p)
    (check-label-targets-declared p)
    (check-has-initial-label p)
    ;; anything else?
    p))

;; Asm -> Void
(define (check-unique-label-decls xs)
  (let ((r (check-duplicates (label-decls xs))))
    (when r
      (error 'prog "duplicate label declaration found: ~v" r))))

;; Asm -> (Listof Symbol)
;; Compute all declared label names
(define (label-decls asm)
  (match asm
    ['() '()]
    [(cons (Label s) asm)
     (cons s (label-decls asm))]
    [(cons (Extern s) asm)
     (cons s (label-decls asm))]    
    [(cons _ asm)
     (label-decls asm)]))

(define (label-symbol? x)
  (and (symbol? x)
       (not (register? x))))

;; Asm -> (Listof Symbol)
;; Compute all uses of label names
(define (label-uses asm)
  (match asm
    ['() '()]
    [(cons (Jmp (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Je (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jne (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jg (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jl (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Call (? label-symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Lea _ (? label-symbol? s)) asm)
     (cons s (label-uses asm))]    
    [(cons _ asm)
     (label-uses asm)]))


;; Asm -> Void
(define (check-label-targets-declared asm)
  (let ((ds (apply set (label-decls asm)))
        (us (apply set (label-uses asm))))

    (let ((undeclared (set-subtract us ds)))
      (unless (set-empty? undeclared)
        (error 'prog "undeclared labels found: ~v" (set->list undeclared))))))

;; Asm -> Void
(define (check-has-initial-label asm)
  (unless (findf Label? asm)
    (error 'prog "no initial label found")))
