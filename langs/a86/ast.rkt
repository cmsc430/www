#lang racket
(provide (all-defined-out))

(define (register? x)
  (and (memq x '(rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)) #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guards

(define check:symbol
  (λ (x n)
    (unless (symbol? x)
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
    (values a1 a2)))

(define check:none
  (λ (n) (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule
  (instruct Name (x ...) guard)
  (begin (provide (struct-out Name))
         (struct Name (x ...)
           #:transparent
           #:guard guard)))


(instruct Label (x)     check:symbol)
(instruct Call  (x)     check:symbol)
(instruct Ret   ()      check:none)
(instruct Mov   (a1 a2) check:src-dest)
(instruct Add   (a1 a2) check:arith)
(instruct Sub   (a1 a2) check:arith)

;; FIXME
(provide (struct-out Cmp))
(struct Cmp (a1 a2) #:prefab)

(instruct Jmp (x)     check:symbol)
(instruct Je  (x)     check:symbol)
(instruct Jne (x)     check:symbol)
(instruct Jl  (x)     check:symbol)
(instruct Jg  (x)     check:symbol)
(instruct And (a1 a2) check:src-dest)
(instruct Or  (a1 a2) check:src-dest)
(instruct Xor (a1 a2) check:src-dest)

;; FIXME
(struct Sal (a1 a2) #:prefab)
(struct Sar (a1 a2) #:prefab)

(instruct Push (a1)
          (λ (a1 n)
            (unless (or (exact-integer? a1) (register? a1))
              (error n "expects exact integer or register; given ~v" a1))
            a1))

(instruct Pop (a1)  check:register)

(instruct Offset (r i)
          (λ (r i n)
            (unless (register? r)
              (error n "expects register as first argument; given ~v" r))
            (unless (exact-integer? i)
              (error n "expects exact integer as second argument; given ~v" i))
            (values r i)))

(define offset? Offset?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define (progn . xs)
  (let ((p (apply seq xs)))
    (check-unique-label-decls xs)
    (check-label-targets-declared xs)
    ;; anything else?
    xs))

;; Asm -> Void
(define (check-unique-label-decls xs)
  (let ((r (check-duplicates (label-decls xs))))
    (when r
      (error 'progn "duplicate label declaration found: ~v" r))))

;; Asm -> (Listof Symbol)
;; Compute all declared label names
(define (label-decls asm)
  (match asm
    ['() '()]
    [(cons (Label s) asm)
     (cons s (label-decls asm))]
    [(cons _ asm)
     (label-decls asm)]))

;; Asm -> (Listof Symbol)
;; Compute all uses of label names
(define (label-uses asm)
  (match asm
    ['() '()]
    [(cons (Jmp (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Je (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jne (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jg (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jl (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Call (? symbol? s)) asm)
     (cons s (label-uses asm))]
    [(cons _ asm)
     (label-uses asm)]))


;; Asm -> Void
(define (check-label-targets-declared asm)
  (let ((ds (apply set (label-decls asm)))
        (us (apply set (label-uses asm))))

    (let ((undeclared (set-subtract us ds)))
      (unless (set-empty? undeclared)
        (error 'progn "undeclared labels found: ~v" (set->list undeclared))))))
