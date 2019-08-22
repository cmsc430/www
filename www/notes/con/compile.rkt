#lang racket
(provide (all-defined-out))

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (con-compile e)
  `(entry
    ,@(con-compile-e e)
    ret))

;; Expr -> Asm
(define (con-compile-e e)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [`(add1 ,e0)
     (let ((c0 (con-compile-e e0)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (con-compile-e e0)))
       `(,@c0
         (sub rax 1)))]
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (con-compile-e e0))
           (c1 (con-compile-e e1))
           (c2 (con-compile-e e2)))
       (match (gen-if-labels)
         [(list if-f if-x)
          `(,@c0
            (cmp rax 0)
            (jne ,if-f)
            ,@c1
            (jmp ,if-x)
            ,if-f
            ,@c2
            ,if-x)]))]))

;; -> [List Label Label]
;; Guaranteed to be unique on each call
(define gen-if-labels
  (let ((i 0))
    (λ ()
      (set! i (add1 i))
      (list (lab "f" i)
            (lab "x" i)))))

;; String Integer -> Symbol
(define (lab s i)
  (string->symbol (string-append "if_" s "_" (number->string i))))
