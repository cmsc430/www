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
    [(? integer? i)
     `((mov rax ,(* i 2)))]
    [(? boolean? b)
     `((mov rax ,(if b #b11 #b01)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (add rax 2)))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (sub rax 2)))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax 0)
         (mov rax #b01) ; #f         
         (jne ,l0)
         (mov rax #b11) ; #t
         ,l0))]
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0))
           (c1 (compile-e e1))
           (c2 (compile-e e2))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b01) ; compare to #f
         (je ,l0)       ; jump to c2 if #f
         ,@c1
         (jmp ,l1)      ; jump past c2
         ,l0
         ,@c2
         ,l1))]))
