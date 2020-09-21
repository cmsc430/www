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
    [(int-e i)
     `((mov rax ,(* i 2)))]
    [(bool-e b)
     `((mov rax ,(if b #b11 #b01)))]
    [(add1-e e1) (let ((c1 (compile-e e1)))
                `(,@c1
                  (add rax 2)))]
    [(sub1-e e1) (let ((c1 (compile-e e1)))
                `(,@c1
                  (sub rax 2)))]
    [(zero?-e e1) (let ((c1 (compile-e e1))
                        (l1 (gensym "nzero")))
                 `(,@c1
                  (cmp rax 0)
                  (mov rax #b01)
                  (jne ,l1)
                  (mov rax #b11)
                  ,l1))]
    [(if-e p t f) (let ((c1 (compile-e p))
                        (c2 (compile-e t))
                        (c3 (compile-e f))
                        (l1 (gensym "if"))
                        (l2 (gensym "if")))
                    `(,@c1
                      (cmp rax #b01) ; compare to false
                      (jne ,l1)
                      ,@c3
                      (jmp ,l2)
                      ,l1
                      ,@c2
                      ,l2))]))
