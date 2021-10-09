#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define r8  'r8)  ; scratch in +, -, make-vector, vector-ref, vector-set!
(define r9  'r9)  ; scratch in assert-type, make-vector, vector-ref, vector-set!
(define r10 'r10) ; scratch in vector-set!
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (Mov rbx rdi) ; recv heap pointer
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        (Sub rsp 8)
        (Jmp 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]))

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
          (seq (assert-integer rax c)
               (Add rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer rax c)
               (Sub rax (imm->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer rax c)
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
          (seq (assert-char rax c)
               (Sar rax char-shift)
               (Sal rax int-shift))]
         ['integer->char
          (seq (assert-codepoint c)
               (Sar rax int-shift)
               (Sal rax char-shift)
               (Xor rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         ['write-byte
          (seq (assert-byte c)
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
          (seq (assert-box rax c)
               (Xor rax type-box)
               (Mov rax (Offset rax 0)))]
         ['car
          (seq (assert-cons rax c)
               (Xor rax type-cons)
               (Mov rax (Offset rax 8)))]
         ['cdr
          (seq (assert-cons rax c)
               (Xor rax type-cons)
               (Mov rax (Offset rax 0)))]
         ['empty? (eq-imm val-empty)]
         ['cons?
          (let ((l1 (gensym)))
            (seq (And rax ptr-mask)
                 (Xor rax type-cons)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['box?
          (let ((l1 (gensym)))
            (seq (And rax ptr-mask)
                 (Xor rax type-box)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['vector?
          (let ((l1 (gensym)))
            (seq (And rax ptr-mask)
                 (Xor rax type-vect)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['vector-length
          (seq (assert-vector rax c)
               (Xor rax type-vect)
               (Mov rax (Offset rax 0))
               (Sal rax int-shift))])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (match p
         ['+
          (seq (Pop r8)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Add rax r8))]
         ['-
          (seq (Pop r8)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Sub r8 rax)
               (Mov rax r8))]
         ['cons
          (seq (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16))]
         ['make-vector
          (let ((loop (gensym))
                (done (gensym))
                (empty (gensym)))
            (seq (Pop r8)
                 (assert-natural r8 c)
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
               (assert-vector r8 c)
               (assert-integer rax c)
               (Cmp rax 0)
               (Jl (error-label c))
               (Xor r8 type-vect)      ; r8 = ptr
               (Mov r9 (Offset r8 0))  ; r9 = len
               (Sar rax int-shift)     ; rax = index
               (Sub r9 1)
               (Cmp r9 rax)
               (Jl (error-label c))
               (Sal rax 3)
               (Add r8 rax)
               (Mov rax (Offset r8 8)))])))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (match p
         ['vector-set!
          (seq (Pop r10)
               (Pop r8)
               (assert-vector r8 c)
               (assert-integer r10 c)
               (Cmp r10 0)
               (Jl (error-label c))
               (Xor r8 type-vect)       ; r8 = ptr
               (Mov r9 (Offset r8 0))   ; r9 = len
               (Sar r10 int-shift)      ; r10 = index
               (Sub r9 1)
               (Cmp r9 r10)
               (Jl (error-label c))
               (Sal r10 3)
               (Add r8 r10)
               (Mov (Offset r8 8) rax)
               (Mov rax val-void))])))

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

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne (error-label c)))))

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

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (imm->bits 0))
         (Jl (error-label c))
         (Cmp rax (imm->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))
       
(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (imm->bits 0))
       (Jl (error-label c))
       (Cmp rax (imm->bits 255))
       (Jg (error-label c))))

(define (assert-natural r c)
  (seq (assert-integer r c)
       (Cmp rax (imm->bits 0))
       (Jl (error-label c))))

;; CEnv -> Label
;; Determine correct error handler label to jump to.
(define (error-label c)
  (match (even? (length c))
    [#t 'raise_error]
    [#f 'raise_error_align]))
