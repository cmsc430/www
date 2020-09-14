#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Expr -> Asm
(define (compile e)
  (append '(entry)
          (compile-e e)
          '(ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(int-e i) `((mov rax ,i))]
    [(add1-e e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (add rax 1)))]
    [(sub1-e e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (sub rax 1)))]))
