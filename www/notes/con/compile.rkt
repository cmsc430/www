#lang racket
(provide (all-defined-out))

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (con-compile e)
  (append '(entry)
          (con-compile-e e '())
          '(ret)))

;; Expr CEnv -> Asm
(define (con-compile-e e c)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,i))))]
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (con-compile-e e0 c))
           (c1 (con-compile-e e1 (cons x c))))
       `(,@c0
         (add rsp -8)
         (mov (offset rsp 0) rax)
         ,@c1
         (add rsp 8)))]
    [`(add1 ,e0)
     (let ((c0 (con-compile-e e0 c)))
       `(,@c0
         (add rax 1)))]
    [`(sub1 ,e0)
     (let ((c0 (con-compile-e e0 c)))
       `(,@c0
         (sub rax 1)))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lookup x cenv))])]))
