#lang racket
(provide (all-defined-out))

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (dupe-compile e)
  (append '(entry)
          (dupe-compile-e e '())
          '(ret)))

;; Expr CEnv -> Asm
(define (dupe-compile-e e c)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,i))))]
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (dupe-compile-e e0 c))
           (c1 (dupe-compile-e e1 (cons x c))))           
       `(,@c0
         (add rsp -8)
         (mov (offset rsp 0) rax)
         ,@c1
         (add rsp 8)))]    
    [`(add1 ,e0)
     (let ((c0 (dupe-compile-e e0 c)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (dupe-compile-e e0 c)))
       `(,@c0
         (sub rax 1)))]
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (dupe-compile-e e0 c))
           (c1 (dupe-compile-e e1 c))
           (c2 (dupe-compile-e e2 c)))
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
