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
    [(if-e i t f) (let ((c1 (compile-e i))
                        (c2 (compile-e t))
                        (c3 (compile-e f))
                        (l1 (gensym "if"))
                        (l2 (gensym "if")))
                    `(,@c1
                      (cmp rax #b01) ; Compare to false now
                      (je ,l1)
                      ,@c2
                      (jmp ,l2)
                      ,l1
                      ,@c3
                      ,l2))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax 0)
         (mov rax #b01) ; #f         
         (jne ,l0)
         (mov rax #b11) ; #t
         ,l0))]))
