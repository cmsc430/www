#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (blackmail-compile e)
  (append '(entry)
          (blackmail-compile-e e)
          '(ret)))

;; Expr -> Asm
(define (blackmail-compile-e e)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [`(add1 ,e)
     (append (blackmail-compile-e e)
             `((add rax 1)))]
    [`(sub1 ,e)
     (append (blackmail-compile-e e)
             `((sub rax 1)))]))
