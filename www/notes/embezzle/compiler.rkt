#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (embezzle-compile e)
  `(entry
    ,@(embezzle-compile-e (embezzle-anf e) '())
    ret))

;; Expr -> ANF
(define (embezzle-anf e)
  (match e
    [(? integer? i) i]
    [(? symbol? x) x]    
    [`(add1 ,e0)
     (let ((a0 (embezzle-anf e0)))
       `(add1 ,a0))]
    [`(sub1 ,e0)
     (let ((a0 (embezzle-anf e0)))
       `(sub1 ,a0))]
    [`(,(? prim2? p) ,e0 ,e1)
     (let ((x0 (generate-variable))
           (x1 (generate-variable))
           (a0 (embezzle-anf e0))
           (a1 (embezzle-anf e1)))
       `(let ((,x0 ,a0))
          (let ((,x1 ,a1))
            (,p ,x0 ,x1))))]
    [`(if ,e0 ,e1 ,e2)
     (let ((a0 (embezzle-anf e0))
           (a1 (embezzle-anf e1))
           (a2 (embezzle-anf e2)))
       `(if ,a0 ,a1 ,a2))]
    [`(let ((,x ,e0)) ,e1)
     (let ((a0 (embezzle-anf e0))
           (a1 (embezzle-anf e1)))
       `(let ((,x ,a0)) ,a1))]))

;; -> Variable
;; Guaranteed to have not appeared before
(define (generate-variable)
  (gensym 't))

;; Symbol -> Boolean
(define (prim2? x)
  (memq x '(+ * -)))

;; ANF CEnv -> Asm
(define (embezzle-compile-e e c)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,i))))]
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (embezzle-compile-e e0 c))
           (c1 (embezzle-compile-e e1 (cons x c))))           
       `(,@c0
         (add rsp -8)
         (mov (offset rsp 0) rax)
         ,@c1
         (add rsp 8)))]    
    [`(add1 ,e0)
     (let ((c0 (embezzle-compile-e e0 c)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (embezzle-compile-e e0 c)))
       `(,@c0
         (sub rax 1)))]
    [`(,(? prim2? p) ,ei0 ,ei1)
     (let ((c0 (embezzle-compile-e ei0 c))
           (a1 (imm-arg ei1 c)))
       `(,@c0
         (,(prim2-op p) rax ,a1)))]    
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (embezzle-compile-e e0 c))
           (c1 (embezzle-compile-e e1 c))
           (c2 (embezzle-compile-e e2 c)))
       (match (gen-if-labels)
         [(list if-t if-f if-x)       
          `(,@c0
            (cmp rax 0)
            (jne ,if-f)
            ,if-t
            ,@c1
            (jmp ,if-x)
            ,if-f
            ,@c2
            ,if-x)]))]))

;; Imm CEnv -> Arg
(define (imm-arg ei c)
  (match ei
    [(? integer?) ei]
    [(? symbol? x)
      (let ((i (lookup x c)))
       `(offset rsp ,i))]))

;; Prim2 -> Symbol
(define (prim2-op p)
  (match p
    ['+ 'add]
    ['- 'sub]
    ['* 'imul]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lookup x cenv))])]))

;; -> [List Label Label Label]
;; Guaranteed to be unique on each call
(define gen-if-labels
  (let ((i 0))
    (λ ()
      (set! i (add1 i))
      (list (lab "t" i)
            (lab "f" i)
            (lab "x" i)))))

;; String Integer -> Symbol
(define (lab s i)
  (string->symbol (string-append "if_" s "_" (number->string i))))
