#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (compile e)
  (append '(entry)
          (compile-e e)
          '(ret)))

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
         (sub rax 1)))]))
