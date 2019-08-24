#lang racket
(provide (all-defined-out))
(require "compile/help.rkt")

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp) 
    (call error)
    ret))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i)
     `((mov rax ,(arithmetic-shift i fixnum-shift)))]
    [(? boolean? b)
     `((mov rax ,(if b true-rep false-rep)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax ,(arithmetic-shift 1 fixnum-shift))))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax ,(arithmetic-shift 1 fixnum-shift))))]             
    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (sete al)
         (movzx rax al)
         (sal rax ,bool-shift)
         (or eax ,false-rep)))]         
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c)))
       (match (gen-if-labels)
         [(list if-f if-x)
          `(,@c0
            (cmp rax ,false-rep)
            (je ,if-f)
            ,@c1
            (jmp ,if-x)
            ,if-f
            ,@c2
            ,if-x)]))]   
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]    
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))
