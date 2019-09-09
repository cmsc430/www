#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e)
    ret))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (sub rax 1)))]
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (compile-e e0))
           (c1 (compile-e e1))
           (c2 (compile-e e2))
           (l0 (gensym "if"))
           (l1 (gensym "if")))
       `(,@c0
         (cmp rax 0)
         (jne ,l0)
         ,@c1
         (jmp ,l1)
         ,l0
         ,@c2
         ,l1))]))
