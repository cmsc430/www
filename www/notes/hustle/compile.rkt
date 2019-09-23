#lang racket
(provide (all-defined-out))

(define int-tag   #b0000) ;  0, 2, 4... integer
(define box-tag   #b0001) ;  1 box
(define pair-tag  #b0011) ;  3 pairs
(define vect-tag  #b0101) ;  5 vectors
(define str-tag   #b0111) ;  7 strings
(define true-tag  #b1001) ;  9
(define false-tag #b1011) ; 11
(define empty-tag #b1101) ; 13
(define char-tag  #b1111) ; 15

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for pairs, etc.

;; type CEnv = (Listof (Maybe Variable))

;; Expr -> Asm
(define (compile e)
  `(entry
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
    [''() `((mov rax ,empty-tag))]
    [`(box ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         (mov (offset rdi 0) rax)
         (mov rax rdi)
         (or rax ,box-tag)
         (add rdi 8) ; bump by 8 bytes
         ))]
    [`(unbox ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert box
         (xor rax ,box-tag)
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
         (or rax ,pair-tag)
         (add rdi 16)))]
    [`(car ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert pair
         (xor rax ,pair-tag)
         (mov rax (offset rax 1))))]
    [`(cdr ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ;; assert pair
         (xor rax ,pair-tag)
         (mov rax (offset rax 0))))]               
    [(? integer? i)
     `((mov rax ,(* i 2)))]
    [(? boolean? b)
     `((mov rax ,(if b true-tag false-tag)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax 2)))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax 2)))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (mov rax ,false-tag) ; #f
         (jne ,l0)
         (mov rax ,true-tag)  ; #t
         ,l0))]
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax ,false-tag) ; compare to #f
         (je ,l0)             ; jump to c2 if #f
         ,@c1
         (jmp ,l1)            ; jump past c2
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
    (and rbx 1)
    (cmp rbx 0)
    (jne err)))
