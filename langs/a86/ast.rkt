#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guards

;; These are used to guard the instruction constructors to reject bad inputs
;; with decent error messages.

(define check:label-symbol
  (λ (a x n)
    (when (register? x)
      (error n "cannot use register as label name; given ~v" x))
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    (unless (label? x)
      (error n "label names must conform to nasm restrictions"))
    (values a x)))

(define check:label-symbol+integer
  (λ (a x c n)
    (check:label-symbol x n)
    (unless (integer? c)
      (error n "expects integer constant; given ~v" c))
    (values a x c)))

(define check:target
  (λ (a x n)
    (unless (or (symbol? x) (offset? x)); either register or label
      (error n "expects symbol; given ~v" x))
    (values a x)))

(define check:cmov
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (register? a2) (offset? a2))
      (error n "expects register or offset; given ~v" a2))
    (values a a1 a2)))

(define check:arith
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (exact-integer? a2) (register? a2) (offset? a2))
      (error n "expects exact integer, register, or offset; given ~v" a2))
    (when (and (exact-integer? a2) (> (integer-length a2) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a2 (integer-length a2)))
    (values a a1 a2)))

(define check:register
  (λ (a a1 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (values a a1)))

(define check:src-dest
  (λ (a a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2))
      (error n "expects register, offset, exact integer, or defined constant; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (exact-integer? a2) (> (integer-length a2) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a2 (integer-length a2)))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (values a a1 a2)))

(define check:mov
  (λ (a a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2))
      (error n "expects register, offset, exact integer, or defined constant; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (exact-integer? a2) (> (integer-length a2) 64))
      (error n "literal must not exceed 64-bits; given ~v (~v bits)" a2 (integer-length a2)))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (values a a1 a2)))

(define check:shift
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (and (exact-integer? a2) (<= 0 a2 63))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,63]; given ~v" a2))
    (values a a1 a2)))

(define check:offset
  (λ (a r i n)
    (unless (or (register? r) (label? r))
      (error n "expects register or label as first argument; given ~v" r))
    (unless (exact-integer? i)
      (error n "expects exact integer as second argument; given ~v" i))
    (values a r i)))

(define check:push
  (λ (a a1 n)
    (unless (or (exact-integer? a1) (register? a1))
      (error n "expects exact integer or register; given ~v" a1))
    (when (and (exact-integer? a1) (> (integer-length a1) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a1 (integer-length a1)))
    (values a a1)))

(define check:lea
  (λ (a dst x n)
    (unless (or (register? dst) (offset? dst))
      (error n "expects register or offset; given ~v" dst))
    (unless (or (label? x) (offset? x) (exp? x))
      (error n "expects label, offset, or expression; given ~v" x))
    (values a dst x)))

(define check:none
  (λ (a n) (values a)))

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

(require racket/struct)
(define current-annotation (make-parameter #f))
(provide instruction-annotation current-annotation)

(struct instruction (annotation))

(define-syntax (instruct stx)
  (syntax-case stx ()
    [(instruct Name (x ...) guard)
     (with-syntax ([Name? (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum #'Name)) "?")))])
     #'(begin (provide Name Name?)
              (define-match-expander Name
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ elts (... ...))
                     #'(%Name _ elts (... ...))]))
                (lambda (stx)
                  (syntax-case stx ()
                    [m (identifier? #'m) #'(λ (x ...) (%Name (current-annotation) x ...))]
                    [(m x ...) #'(%Name (current-annotation) x ...)])))
              (struct %Name instruction (x ...)
                #:reflection-name 'Name
                #:transparent
                #:guard guard
                #:methods gen:equal+hash
                [(define equal-proc (λ (i1 i2 equal?)
                                      (equal? (struct->vector i1)
                                              (struct->vector i2))))
                 (define hash-proc  (λ (i hash) (hash (struct->vector i))))
                 (define hash2-proc (λ (i hash) (hash (struct->vector i))))]

                #:property prop:custom-print-quotable 'never
                #:methods gen:custom-write
                [(define write-proc
		   (instr-print 'Name)
                   #;(make-constructor-style-printer
                    (lambda (obj) 'Name)
                    (lambda (obj)
                      (rest (rest (vector->list (struct->vector obj)))))))])
              (define Name? %Name?)))]))

(define (instr-print type)
  (lambda (instr port mode)
    (if (number? mode)
        (write-string "(" port)
        (write-string "#(struct:" port))
    (write-string (symbol->string type) port)
    (let ([recur (case mode
                   [(#t) write]
                   [(#f) display]
                   [else (lambda (p port) (print p port mode))])])
        (for-each (lambda (e)
                    (write-string " " port)
                    (recur e port))
                  (rest (rest (vector->list (struct->vector instr))))))
    (if (number? mode)
        (write-string ")" port)
        (write-string ")" port))))


(instruct Text   ()        check:none)
(instruct Data   ()        check:none)

(instruct Global (x)       check:label-symbol)
(instruct Label  (x)       check:label-symbol)
(instruct Call   (x)       check:target)
(instruct Ret    ()        check:none)
(instruct Mov    (dst src) check:mov)
(instruct Add    (dst src) check:arith)
(instruct Sub    (dst src) check:arith)
(instruct Cmp    (a1 a2)   check:src-dest)
(instruct Jmp    (x)       check:target)
(instruct Jz     (x)       check:target)
(instruct Jnz    (x)       check:target)
(instruct Je     (x)       check:target)
(instruct Jne    (x)       check:target)
(instruct Jl     (x)       check:target)
(instruct Jle    (x)       check:target)
(instruct Jg     (x)       check:target)
(instruct Jge    (x)       check:target)
(instruct Jo     (x)       check:target)
(instruct Jno    (x)       check:target)
(instruct Jc     (x)       check:target)
(instruct Jnc    (x)       check:target)
(instruct Cmovz  (dst src) check:cmov)
(instruct Cmovnz (dst src) check:cmov)
(instruct Cmove  (dst src) check:cmov)
(instruct Cmovne (dst src) check:cmov)
(instruct Cmovl  (dst src) check:cmov)
(instruct Cmovle (dst src) check:cmov)
(instruct Cmovg  (dst src) check:cmov)
(instruct Cmovge (dst src) check:cmov)
(instruct Cmovo  (dst src) check:cmov)
(instruct Cmovno (dst src) check:cmov)
(instruct Cmovc  (dst src) check:cmov)
(instruct Cmovnc (dst src) check:cmov)
(instruct And    (dst src) check:src-dest)
(instruct Or     (dst src) check:src-dest)
(instruct Xor    (dst src) check:src-dest)
(instruct Sal    (dst i)   check:shift)
(instruct Sar    (dst i)   check:shift)
(instruct Shl    (dst i)   check:shift)
(instruct Shr    (dst i)   check:shift)
(instruct Push   (a1)      check:push)
(instruct Pop    (a1)      check:register)
(instruct Pushf  ()        check:none)
(instruct Popf   ()        check:none)
(instruct Lea    (dst x)   check:lea)
(instruct Not    (x)       check:register)
(instruct Div    (den)     check:register)

(instruct Offset (r i)     check:offset)        ;; May need to make this not an instruction
(instruct Extern (x)       check:label-symbol)

(instruct Equ    (x v)     check:label-symbol+integer)
(instruct Const  (x)       check:label-symbol)

;; IMPROVE: do more checking
(instruct Db (x) (lambda (a x n) (values a x)))
(instruct Dw (x) (lambda (a x n) (values a x)))
(instruct Dd (x) (lambda (a x n) (values a x)))
(instruct Dq (x) (lambda (a x n) (values a x)))

(provide (struct-out Plus))
(struct Plus (e1 e2) #:transparent)

(provide exp?)
(define (exp? x)
  (or (Offset? x)
      (and (Plus? x)
           (exp? (Plus-e1 x))
           (exp? (Plus-e2 x)))
      (symbol? x)
      (integer? x)))

(provide offset? register? label? 64-bit-integer? 32-bit-integer?)

(define offset? Offset?)

(define (register? x)
  (and (memq x '(cl eax rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
       #t))

(define (64-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-length x) 64)))

(define (32-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-length x) 32)))

(define (label? x)
  (and (symbol? x)
       (nasm-label? x)
       (not (register? x))))

(provide (rename-out [a86:instruction? instruction?]))
(define (a86:instruction? x)
  (or (instruction? x)
      (Comment? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction sequencing and program error checking

(provide/contract
 [seq   (-> (or/c a86:instruction? (listof a86:instruction?)) ...
            (listof a86:instruction?))]
 [prog (-> (or/c a86:instruction? (listof a86:instruction?)) ...
           (listof a86:instruction?))])

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
    (check-initial-label-global p)
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

;; Symbol -> Boolean
(define (nasm-label? s)
  (regexp-match #rx"^[a-zA-Z._?][a-zA-Z0-9_$#@~.?]*$" (symbol->string s)))

;; Asm -> (Listof Symbol)
;; Compute all uses of label names
(define (label-uses asm)
  (match asm
    ['() '()]
    [(cons (Jmp (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Je (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jne (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jg (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jge (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jl (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jle (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Call (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Lea _ (? label? s)) asm)
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

;; Asm -> Void
(define (check-initial-label-global asm)
  (match (findf Label? asm)
    [(Label init)
     (unless (member init (map (lambda (i) (match i [(Global l) l]))
                               (filter Global? asm)))
       (error 'prog "initial label undeclared as global: ~v" init))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol to Label

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
;; Guarantees that (eq? s1 s2) <=> (eq? (symbol->label s1) (symbol->label s1))
(provide symbol->label)
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
