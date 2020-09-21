#lang racket
(provide (all-defined-out))

(require "ast.rkt")
(require "primitives.rkt")

;; Expr -> Asm
(define (compile e)
  (append '(entry)
          (compile-e e)
          '(ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(int-e i) `((mov rax ,i))]
    [(add1-e e1) (let ((c1 (compile-e e1))) ; c1 : [ASM]
                `(,@c1
                  (add rax 1)))]
    [(sub1-e e1) (let ((c1 (compile-e e1)))
                `(,@c1
                  (sub rax 1)))]
    [(get-i)  get-int-asm]))
