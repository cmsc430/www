#lang racket
(provide (all-defined-out))

(require "ast.rkt")

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
    [(int-e i)
     `((mov rax ,(* i 2)))]
    [(bool-e b)
     `((mov rax ,(if b #b11 #b01)))]
    [(prim-e p e)
     (let ((c0 (compile-e e c)))
       `(,@c0
         ,@(compile-prim p c)))]
    [(if-e p t f)
     (let ((c0 (compile-e p c))
           (c1 (compile-e t c))
           (c2 (compile-e f c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b01) ; compare to #f
         (je ,l0)       ; jump to c2 if #f
         ,@c1
         (jmp ,l1)      ; jump past c2
         ,l0
         ,@c2
         ,l1))]
    [(var-e v) 
      (let ((pos (lookup v c)))
        `((mov rax (offset rsp ,(- (add1 pos))))))]
    [(let-e (list (binding v def)) body)
      (let ((c-def  (compile-e def c))
            (c-body (compile-e body (cons v c))))
           `(,@c-def
            (mov (offset rsp ,(- (add1 (length c)))) rax)
            ,@c-body))]))

(define (compile-prim p c)
  (match p
    [`add1
         `(,@assert-integer
          (add rax 2))]
    [`sub1
         `(,@assert-integer
          (sub rax 2))]
    [`zero?
     (let ((l0 (gensym))
           (l1 (gensym)))
        `(,@assert-integer
         (cmp rax 0)
         (mov rax #b01) ; #f
         (jne ,l0)
         (mov rax #b11) ; #t
         ,l0))]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (symbol=? x y)
       [#t (length rest)]
       [#f (lookup x rest)])]))

(define assert-integer
  `((mov rbx rax)
    (and rbx 1)
    (cmp rbx 0)
    (jne err)))
