#lang racket
(provide (all-defined-out))
(require "compile/help.rkt")

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e)
    ret
    err
    (push rbp) ; doesn't seem to matter what we push
    (call error)
    #;(pop rbp)))

(define true-rep  #b10011111)
(define false-rep #b00011111)
(define fixnum-mask #b11)
(define fixnum-shift 2)
(define bool-shift   8)

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(? integer? i)
     `((mov rax ,(arithmetic-shift i fixnum-shift)))]
    [(? boolean? b)
     `((mov rax ,(if b true-rep false-rep)))]
    [#t `((mov rax ,true-rep))]
    [#f `((mov rax ,false-rep))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         ,@assert-integer
         (add rax ,(arithmetic-shift 1 fixnum-shift))))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         ,@assert-integer
         (sub rax ,(arithmetic-shift 1 fixnum-shift))))]             
    [`(zero? ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (sete al)
         (movzx rax al)
         (sal rax ,bool-shift)
         (or eax ,false-rep)))]         
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0))
           (c1 (compile-e e1))
           (c2 (compile-e e2)))
       (match (gen-if-labels)
         [(list if-f if-x)
          `(,@c0
            (cmp rax ,false-rep)
            (je ,if-f)
            ,@c1
            (jmp ,if-x)
            ,if-f
            ,@c2
            ,if-x)]))]))

(define assert-integer
  `((mov rbx rax)
    (and rbx ,fixnum-mask)
    (cmp rbx 0)
    (jne err)))
