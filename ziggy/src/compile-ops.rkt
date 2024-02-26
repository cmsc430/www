#lang crook
{:= B C D0 D0.A D1 E0 E1 F H0 H1 I J K L}
(provide {:> E0} compile-op0 compile-op1 {:> F} compile-op2 {:> H1} compile-op3 {:> F} pad-stack {:> L} assert-proc)
(require "ast.rkt")
{:> D0} (require "types.rkt")
(require a86/ast)

(define rax 'rax)
{:> H1} (define eax 'eax) {:> H1} ; 32-bit load/store
{:> H0} (define rbx 'rbx) {:> H0} ; heap
{:> E0} (define rdi 'rdi) {:> E0} ; arg
{:> F}  (define r8  'r8)  {:> F}  ; scratch in op2
{:> D0} (define r9  'r9)  {:> E0} ; scratch
{:> H1} (define r10 'r10) {:> H1} ; scratch

{:> F}  (define r15 'r15) {:> F}  ; stack pad (non-volatile)
{:> F}  (define rsp 'rsp) {:> F}  ; stack

{:> E0} ;; Op0 -> Asm
{:> E0}
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq {:> F} pad-stack (Call 'read_byte) {:> F} unpad-stack)]
    ['peek-byte (seq {:> F} pad-stack (Call 'peek_byte) {:> F} unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    {:> B D0}
    ['add1 (Add rax 1)]
    {:> B D0}
    ['sub1 (Sub rax 1)]
    {:> D0 E1}
    ['add1 (Add rax (value->bits 1))]
    {:> E1}
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    {:> D0 E1}
    ['sub1 (Sub rax (value->bits 1))]
    {:> E1}
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    {:> D0}
    ['zero?
     {:> D0 D1}
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))
     {:> D1}
     (seq {:> E1} (assert-integer rax)
          (Cmp rax 0)
          if-equal)]
    {:> D1}
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    {:> D1}
    ['char->integer
     (seq {:> E1} (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    {:> D1}
    ['integer->char
     (seq {:> E1} (assert-codepoint)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    {:> E0}
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          if-equal)]
    {:> E0}
    ['write-byte
     (seq {:> E1} assert-byte
          {:> F} pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          {:> F} unpad-stack)]

    {:> H0}
    ['box
     (seq (Mov (Offset rbx 0) rax) ; memory write
          (Mov rax rbx)            ; put box in rax
          (Or rax type-box)        ; tag as a box
          (Add rbx 8))]
    
    {:> H0}
    ['unbox
     (seq (assert-box rax)
          (Xor rax type-box)
          (Mov rax (Offset rax 0)))]
    {:> H0}
    ['car
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]
    {:> H0}
    ['cdr
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]
    
    {:> H0}
    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    {:> H0}
    ['cons? (type-pred ptr-mask type-cons)]
    {:> H0}
    ['box?  (type-pred ptr-mask type-box)]
    {:> H1}
    ['vector? (type-pred ptr-mask type-vect)]
    {:> H1}
    ['string? (type-pred ptr-mask type-str)]
    {:> H1}
    ['vector-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-vector rax)
            (Xor rax type-vect)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    {:> H1}
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]))


{:> F} ;; Op2 -> Asm
{:> F}
(define (compile-op2 p)
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
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-lt)]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-equal)]
    {:> H0}
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    {:> H0}
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    {:> H1}
    ['make-vector ;; size value
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8) ;; r8 = size
            (assert-natural r8)
            (Cmp r8 0) ; special case empty vector
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-vect)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-vect)
            (Label done)))]
    {:> H1}
    ['vector-ref ; vector index
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp r8 type-vect)
          (Je 'err) ; special case for empty vector
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]
    {:> H1}
    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-str)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Offset rbx 0) eax)
            (Add rbx 4)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-str)
            (Label done)))]
    {:> H1}
    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp r8 type-str)
          (Je 'err) ; special case for empty string
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Or rax type-char))]))

{:> H1} ;; Op3 -> Asm
{:> H1}
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'err)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'err)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax (value->bits (void))))]))


{:> D1} ;; -> Asm
{:> D1} ;; set rax to #t or #f if comparison flag is equal
{:> D1}
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

{:> F} ;; -> Asm
{:> F} ;; set rax to #t or #f if comparison flag is less than
{:> F}
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))

{:> E1}
(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'err))))

{:> E1}
(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

{:> E1}
(define assert-integer
  (assert-type mask-int type-int))
{:> E1}
(define assert-char
  (assert-type mask-char type-char))
{:> H0}
(define assert-box
  (assert-type ptr-mask type-box))
{:> H0}
(define assert-cons
  (assert-type ptr-mask type-cons))
{:> H1}
(define assert-vector
  (assert-type ptr-mask type-vect))
{:> H1}
(define assert-string
  (assert-type ptr-mask type-str))
{:> L}
(define assert-proc
  (assert-type ptr-mask type-proc))

{:> E1}
(define (assert-codepoint)
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (value->bits 0))
         (Jl 'err)
         (Cmp rax (value->bits 1114111))
         (Jg 'err)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

{:> E1}
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (value->bits 0))
       (Jl 'err)
       (Cmp rax (value->bits 255))
       (Jg 'err)))

{:> H1}
(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'err)))

{:> F} ;; Asm
{:> F} ;; Dynamically pad the stack to be aligned for a call
{:> F}
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

{:> F} ;; Asm
{:> F} ;; Undo the stack alignment after a call
{:> F}
(define unpad-stack
  (seq (Add rsp r15)))
