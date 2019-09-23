#lang racket
(provide (all-defined-out))

;; An immediate is anything ending in #b0000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)

(define imm-shift        (+ 3 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b000 result-shift))
(define imm-type-true    (arithmetic-shift #b001 result-shift))
(define imm-type-false   (arithmetic-shift #b010 result-shift))
(define imm-type-empty   (arithmetic-shift #b011 result-shift))
(define imm-type-char    (arithmetic-shift #b100 result-shift))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))

;; Expr -> Asm
(define (compile e)
  `(entry
    (sar rdi 4)  ;; align on quad boundary
    (sal rdi 4)
    (add rdi 16)
    ,@(compile-e e '())
    ret
    err
    (push rbp)
    (call error)
    ret))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [''() `((mov rax ,imm-type-empty))]
    [`(box ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         (mov (offset rdi 0) rax)
         (mov rax rdi)
         (or rax ,type-box)
         (add rdi 8)))] ; allocate 8 bytes
    [`(unbox ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert box
         (xor rax ,type-box)
         (mov rax (offset rax 0))))]
    [`(cons ,e0 ,e1)
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
         (add rdi 16)))]
    [`(car ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert pair
         (xor rax ,type-pair)
         (mov rax (offset rax 1))))]
    [`(cdr ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert pair
         (xor rax ,type-pair)
         (mov rax (offset rax 0))))]               
    [(? integer? i)
     `((mov rax ,(arithmetic-shift i imm-shift)))]
    [(? boolean? b)
     `((mov rax ,(if b imm-type-true imm-type-false)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax ,(arithmetic-shift 1 imm-shift))))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax ,(arithmetic-shift 1 imm-shift))))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (mov rax ,imm-type-false)
         (jne ,l0)
         (mov rax ,imm-type-true)
         ,l0))]
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax ,imm-type-false)
         (je ,l0)
         ,@c1
         (jmp ,l1)
         ,l0
         ,@c2
         ,l1))]
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]

    [`(+ ,e0 ,e1)
     (let ((c1 (compile-e e1 c))
           (c0 (compile-e e0 (cons #f c))))
       `(,@c1
         ,@assert-integer
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c0
         ,@assert-integer
         (add rax (offset rsp ,(- (add1 (length c)))))))]

    [`(- ,e0 ,e1)
     (let ((c1 (compile-e e1 c))
           (c0 (compile-e e0 (cons #f c))))
       `(,@c1
         ,@assert-integer
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c0
         ,@assert-integer
         (sub rax (offset rsp ,(- (add1 (length c)))))))]))

;; code for "abc"
'((mov (offset rdi 0)
       (integer->bits 3))
  (mov (offset rdi 1)
       (compile-char #\a))
  (mov (offset rdi 1)
       (char->bits #\b))
  (mov (offset-rdi 2)
       (char->bits #\c))
  (mov rdi rax)
  (or rax ,str-tag))
  
       


;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define assert-integer
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx 0)
    (jne err)))
