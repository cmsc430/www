#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define r8  'r8)  ; scratch in make-string, string-set!, string-ref, +, -
(define r9  'r9)  ; scratch in make-string, string-set!
(define r10 'r10) ; scratch in assert-type
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; need runtime hook
;; Ops on memory
;; clobbering callee saved registers

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (Push rbx)
        (Mov rbx rdi) ; recv heap pointer
        (compile-e e '())
        (Pop rbx)
        (Ret)
        ;; Error handler
        (Label 'err)
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Str s)            (compile-string s)]    
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

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (seq (Mov rax (imm->bits len))
         (Mov (Offset rbx 0) rax)
         (compile-string-chars (string->list s) 1)
         (Mov rax rbx)
         (Or rax type-str)
         (Add rbx (* 8 (add1 len))))))

;; [Listof Char] Nat -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() '()]
    [(cons c cs)
     (seq (Mov rax (imm->bits c))
          (Mov (Offset rbx (* i 8)) rax)
          (compile-string-chars cs (add1 i)))]))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (move-env Sub c)
                     (Call 'read_byte)
                     (move-env Add c))]
    ['peek-byte (seq (move-env Sub c)
                     (Call 'peek_byte)
                     (move-env Add c))]))

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
               (move-env Sub c)
               (Mov rdi rax)
               (Call 'write_byte)
               (move-env Add c)
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
         ['empty? (eq-imm val-empty)]
         ['string?
          (type-pred ptr-mask type-str)]
         ['string-length
          (seq (assert-str rax)
               (Xor rax type-str)
               (Mov rax (Offset rax 0)))])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (let ((i (next c)))
    (seq (compile-e e1 c)
         (Mov (Offset rsp i) rax)
         (compile-e e2 (cons #f c))       
         (match p
           ['+
            (seq (Mov r8 (Offset rsp i))
                 (assert-integer r8)
                 (assert-integer rax)
                 (Add rax r8))]
           ['-
            (seq (Mov r8 (Offset rsp i))
                 (assert-integer r8)
                 (assert-integer rax)
                 (Sub r8 rax)
                 (Mov rax r8))]
           ['eq?
            (let ((l (gensym)))
              (seq (Cmp rax (Offset rsp i))
                   (Mov rax val-true)
                   (Je l)
                   (Mov rax val-false)
                   (Label l)))]
           ['cons
            (seq (Mov (Offset rbx 0) rax)
                 (Mov rax (Offset rsp i))
                 (Mov (Offset rbx 8) rax)
                 (Mov rax rbx)
                 (Or rax type-cons)
                 (Add rbx 16))]
           ;; This stuff makes assumptions about integers being tagged with #b0000          
           ['string-ref
            (seq (Mov r8 (Offset rsp i))
                 (assert-str r8)
                 (assert-integer rax)
                 (Xor r8 type-str)
                 ;assert-bound
                 (Sar rax 1)
                 (Add r8 rax)
                 (Mov rax (Offset r8 8)))]
           ['make-string
            (let ((done (gensym))
                  (loop (gensym)))
              (seq (assert-char rax)
                   (assert-integer (Offset rsp i))
                   (Mov r8 (Offset rsp i))
                   ; assert-nat rsp[i]
                   (Mov (Offset rbx 0) r8) ; Write length
                   (Mov r9 r8)            ; Save length
                   (Mov r8 rax)            ; Save char
                   
                   (Mov rax rbx)            ; Put str addr in rax 
                   (Or rax type-str)         ; Tag                   
                   (Add rbx 8)               ; Allocate cell for length
                   ;; DVH: I don't understand why not shifting rcx
                   ;; and decrementing by (imm->bits 1) doesn't work
                   (Sar r9 int-shift)
                   (Label loop)               ; Initialize string
                   (Cmp r9 0)
                   (Je done)
                   ;(Mov rbx (imm->bits #\a))
                   (Mov (Offset rbx 0) r8)
                   (Add rbx 8)
                   (Sub r9 1)
                   (Jmp loop)
                   (Label done)))]))))

;; Op2 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (let ((i (next c))
        (j (next (cons #f c))))        
    (seq (compile-e e1 c)
         (Mov (Offset rsp i) rax)
         (compile-e e2 (cons #f c))
         (Mov (Offset rsp j) rax)
         (compile-e e3 (cons #f (cons #f c)))
         (match p
           ['string-set!
            (seq (Mov r9 (Offset rsp i))
                 (Mov r8 (Offset rsp j))                 
                 (assert-str r9)
                 (assert-integer r8)
                 (assert-char rax)
                 ; assert-bounds j i
                 (Xor r9 type-str)
                 (Sar r8 (- int-shift 3))                 
                 (Add r9 r8)
                 (Mov (Offset r9 8) rax)
                 (Mov rax val-void))]))))

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
       (Mov (Offset rsp (next c)) rax)
       (compile-e e2 (cons x c))))

;; CEnv -> Integer
(define (next c)
  (- (* 8 (add1 (length c)))))

;; CEnv -> Asm
;; Adjust 'rsp to save/restore current environment
;; dir is either Sub (to save) or Add (to restore)
(define (move-env dir c)
  (let ((l (length c)))
    (cond [(zero? l) (seq)] ; special case
          [(even? l) (seq (dir rsp (* 8 l)))]
          [(odd? l)  (seq (dir rsp (* 8 (add1 l))))])))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t (- (* 8 (length cenv)))]
       [#f (lookup x rest)])]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r10 arg)
         (And r10 mask)
         (Cmp r10 type)
         (Jne 'err))))

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
(define assert-str
  (assert-type ptr-mask type-str))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (imm->bits 0))
         (Jl 'err)
         (Cmp rax (imm->bits 1114111))
         (Jg 'err)
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (imm->bits 0))
       (Jl 'err)
       (Cmp rax (imm->bits 255))
       (Jg 'err)))
       
