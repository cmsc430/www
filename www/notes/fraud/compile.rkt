#lang racket
(provide (all-defined-out))
(require "compile/help.rkt")

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp)
    (call error)
    ret))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i)
     `((mov rax ,(* i 2)))]
    [(? boolean? b)
     `((mov rax ,(if b #b11 #b01)))]
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
         (mov rax #b01) ; #f
         (jne ,l0)
         (mov rax #b11) ; #t
         ,l0))]
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b01) ; compare to #f
         (je ,l0)       ; jump to c2 if #f
         ,@c1
         (jmp ,l1)      ; jump past c2
         ,l0
         ,@c2
         ,l1))]
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))
