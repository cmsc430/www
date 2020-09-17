#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e)
    ret))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(int-e i) `((mov rax ,i))]
    [(add1-e e1) (let ((c1 (compile-e e1)))
                `(,@c1
                  (add rax 1)))]
    [(sub1-e e1) (let ((c1 (compile-e e1)))
                `(,@c1
                  (sub rax 1)))]
    [(if-e i t f) (let ((c1 (compile-e i))
                        (c2 (compile-e t))
                        (c3 (compile-e f))
                        (l1 (gensym "if"))
                        (l2 (gensym "if")))
                    `(,@c1
                      (cmp rax 0) ; zero? <result of executing code for i>
                      (jne ,l1)
                      ,@c2
                      (jmp ,l2)
                      ,l1
                      ,@c3
                      ,l2))]))
