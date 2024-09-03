#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in op2
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch

(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq pad-stack (Call 'peek_byte) unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (Cmp rax 0)
          if-equal)]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          if-equal)]
    ['write-byte
     (seq assert-byte
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack)]

    ['box
     (seq (Mov (Offset rbx 0) rax) ; memory write
          (Mov rax rbx)            ; put box in rax
          (Or rax type-box)        ; tag as a box
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

    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons? (type-pred ptr-mask type-cons)]
    ['box?  (type-pred ptr-mask type-box)]
    ['vector? (type-pred ptr-mask type-vect)]
    ['string? (type-pred ptr-mask type-str)]
    ['vector-length
     (let ((end (gensym)))
       (seq (assert-vector rax)
            (Xor rax type-vect)
            (Cmp rax 0)
            (Je end)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Label end)))]
    ['string-length
     (let ((end (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je end)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Label end)))]))


;; Op2 -> Asm
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
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['make-vector
     ;; size is first element on stack
     ;; value is in rax
     (let ((end (gensym 'end))
           (loop (gensym 'loop)))
       (seq (Pop r8)
            (assert-natural r8)
            (Cmp r8 0) ; special case for empty vector
            (Mov r10 0)
            (Je end)
            (Sar r8 4)
            (Mov r10 rbx)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)
            (Label loop)
            (Cmp r8 0)
            (Je end)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Jmp loop)
            (Label end)
            (Mov rax r10)
            (Xor rax type-vect)))]
    ['vector-ref
     ;; vector is first element on stack
     ;; index is in rax
     (seq (Pop r8)
          (assert-vector r8)
          (assert-natural rax)
          (Sar rax 4)             ; decode
          (Xor r8 type-vect)      ; untag
          (Cmp r8 0)
          (Je 'err)
          (Cmp rax (Offset r8 0)) ; check bound
          (Jge 'err)
          (Sal rax 3) ; mult by 8 for byte offset
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]
    ['make-string
     ;; size is first element on stack
     ;; character is in rax
     (let ((end (gensym 'end))
           (loop (gensym 'loop)))
       (seq
        (Pop r8)
        (assert-natural r8)
        (assert-char rax)
        (Sar rax 5)
        (Cmp r8 0) ; special case for empty string
        (Mov r10 0)
        (Je end)
        (Sar r8 4)
        (Mov r10 rbx)
        (Mov (Offset rbx 0) r8)
        (Add rbx 8)
        (Add r8 1) ; adds 1
        (Sar r8 1) ; when
        (Sal r8 1) ; len is odd
        (Label loop)
        (Cmp r8 0)
        (Je end)
        (Mov (Offset rbx 0) eax)
        (Add rbx 4)
        (Sub r8 1)
        (Jmp loop)
        (Label end)
        (Mov rax r10)
        (Xor rax type-str)))]
    ['string-ref
     ;; string is first element on stack
     ;; index is in rax
     (seq (Pop r8)
          (assert-string r8)
          (assert-natural rax)
          (Sar rax 4)
          (Xor r8 type-str)
          (Cmp r8 0)
          (Je 'err)
          (Cmp rax (Offset r8 0))
          (Jge 'err)
          (Sal rax 2)
          (Add r8 rax)
          (Mov eax (Offset r8 8))
          (Sal rax 5)
          (Xor rax type-char))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     ;; vector is second element on stack
     ;; index is first element on stack
     ;; value is in rax
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-natural r10)
          (Sar r10 4)             ; decode
          (Xor r8 type-vect)      ; untag
          (Cmp rax 0)
          (Je 'err)
          (Cmp r10 (Offset r8 0)) ; check bound
          (Jge 'err)
          (Sal r10 3) ; mult by 8 for byte offset
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax (value->bits (void))))]))

;; -> Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; -> Asm
;; set rax to #t or #f if comparison flag is less than
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'err))))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

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

(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (value->bits 0))
       (Jl 'err)
       (Cmp rax (value->bits 255))
       (Jg 'err)))

(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'err)))

;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))

