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
    [(? symbol? x)
     (match (lookup x c)
       [i `((mov rax (offset rsp ,i)))])]
    [`(let ((,x ,e0)) ,e1)
     (append (con-compile-e e0 c)
             '((add rsp -8)
               (mov (offset rsp 0) rax))
             (con-compile-e e1 (cons x c))
             '((add rsp 8)))]    
    [(? integer? i) `((mov rax ,i))]
    [`(add1 ,e)
     (append (con-compile-e e c)
             `((add rax 1)))]
    [`(sub1 ,e)
     (append (con-compile-e e c)
             `((sub rax 1)))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lookup x cenv))])]))
