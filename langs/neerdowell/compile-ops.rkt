#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "utils.rkt" a86/ast)

(define rax 'rax) ; return
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg1
(define rsi 'rsi) ; arg2
(define rdx 'rdx) ; arg3
(define r8  'r8)  ; scratch
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r12 'r12) ; save across call to memcpy
(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op -> Asm
(define (compile-op p)
  (match p
    ;; Op0
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq pad-stack
                     (Call 'read_byte)
                     unpad-stack)]
    ['peek-byte (seq pad-stack
                     (Call 'peek_byte)
                     unpad-stack)]
    ;; Op1
    ['add1
     (seq (assert-integer rax)
          (Add rax (imm->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (imm->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (eq-imm 0))]
    ['char?
     (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-imm eof)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack
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
    ['empty? (eq-imm '())]
    ['box?
     (type-pred ptr-mask type-box)]
    ['cons?
     (type-pred ptr-mask type-cons)]
    ['vector?
     (type-pred ptr-mask type-vect)]
    ['string?
     (type-pred ptr-mask type-str)]
    ['symbol?
     (type-pred ptr-mask type-symb)]
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
            (Label done)))]
    ['string->symbol
     (seq (assert-string rax)
          (Xor rax type-str)
          (Mov rdi rax)
          pad-stack
          (Call 'intern_symbol)
          unpad-stack
          (Or rax type-symb))]
    ['symbol->string
     (seq (assert-symbol rax)
          (Xor rax type-symb)
          char-array-copy
          (Or rax type-str))]
    ['string->uninterned-symbol
     (seq (assert-string rax)
          (Xor rax type-str)
          char-array-copy
          (Or rax type-symb))]

    ;; Op2
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
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Jl true)
                 (Mov rax val-false)
                 (Label true))))]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Je true)
                 (Mov rax val-false)
                 (Label true))))]
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (eq r8 rax))]
    ['make-vector
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
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

    ['vector-ref
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]

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

    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Or rax type-char))]

    ['struct?
     (let ((f (gensym))
           (t (gensym)))
       (seq (Pop r8)
            ; (assert-symbol r8) ; don't need to do this we generated the code
            (Mov r9 rax)
            (And r9 ptr-mask)
            (Cmp r9 type-struct)
            (Jne f)
            (Xor rax type-struct)
            (Mov rax (Offset rax 0))
            (Cmp r8 rax)
            (Mov rax (imm->bits #t))
            (Jne f)
            (Jmp t)
            (Label f)
            (Mov rax (imm->bits #f))
            (Label t)))]
      
    ;; Op3
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'raise_error_align)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax val-void))]

    ['struct-ref ; symbol, int, struct
     (seq (Pop r8)
          (Pop 'r11)
          (assert-struct rax)
          ;(assert-integer r8)
          (Xor rax type-struct)
          (Mov r10 (Offset rax 0))
          (Cmp 'r11 r10)
          (Jne 'raise_error_align)
          (Sar r8 int-shift)
          (Add r8 1)
          (Sal r8 3)
          (Add rax r8)
          (Mov rax (Offset rax 0)))]))

(define (compile-make-struct n)
  (seq (compile-make-struct/a n 1)
       (Mov rax rbx)
       (Or rax type-struct)
       (Add rbx (* 8 n))))

(define (compile-make-struct/a n i)
  (if (= n i)
      (seq (Mov (Offset rbx (* 8 (- n i))) rax))
      (seq (Mov (Offset rbx (* 8 (- n i))) rax)
           (Pop rax)
           (compile-make-struct/a n (add1 i)))))

;; Asm
;; Copy sized array of characters pointed to by rax
(define char-array-copy
  (seq (Mov rdi rbx)            ; dst
       (Mov rsi rax)            ; src
       (Mov rdx (Offset rax 0)) ; len
       (Add rdx 1)              ; #words = 1 + (len+1)/2
       (Sar rdx 1)
       (Add rdx 1)
       (Sal rdx 3)              ; #bytes = 8*#words
       (Mov r12 rdx)            ; save rdx before destroyed
       pad-stack
       (Call 'memcpy)
       unpad-stack
       ; rbx should be preserved by memcpy
       ;(Mov rbx rax) ; dst is returned, install as heap pointer
       (Add rbx r12)))                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error_align))))

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
(define assert-vector
  (assert-type ptr-mask type-vect))
(define assert-string
  (assert-type ptr-mask type-str))
(define assert-symbol
  (assert-type ptr-mask type-symb))
(define assert-proc
  (assert-type ptr-mask type-proc))
(define assert-struct
  (assert-type ptr-mask type-struct))

(define (assert-codepoint r)
  (let ((ok (gensym)))
    (seq (assert-integer r)
         (Cmp r (imm->bits 0))
         (Jl 'raise_error_align)
         (Cmp r (imm->bits 1114111))
         (Jg 'raise_error_align)
         (Cmp r (imm->bits 55295))
         (Jl ok)
         (Cmp r (imm->bits 57344))
         (Jg ok)
         (Jmp 'raise_error_align)
         (Label ok))))

(define (assert-byte r)
  (seq (assert-integer r)
       (Cmp r (imm->bits 0))
       (Jl 'raise_error_align)
       (Cmp r (imm->bits 255))
       (Jg 'raise_error_align)))

(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (imm->bits 0))
       (Jl 'raise_error_align)))

;; Value -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax (imm->bits imm))
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

(define (eq ir1 ir2)
  (let ((l1 (gensym)))
    (seq (Cmp ir1 ir2)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))
