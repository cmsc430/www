#lang racket

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
    [(? integer? i)        (compile-integer i)]
    [(? boolean? b)        (compile-boolean b)]
    [(? symbol? x)         (compile-variable x c)]    
    [`(add1 ,e0)           (compile-add1 e0 c)]
    [`(sub1 ,e0)           (compile-sub1 e0 c)]
    [`(zero? ,e0)          (compile-zero? e0 c)]
    [`(if ,e0 ,e1 ,e2)     (compile-if e0 e1 e2 c)]
    [`(let ((,x ,e0)) ,e1) (compile-let x e0 e1 c)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1)          (compile-- e0 e1 c)]))

;; Integer -> Asm
(define (compile-integer i)
  `((mov rax ,(* i 2))))

;; Boolean -> Asm
(define (compile-boolean b)
  `((mov rax ,(if b #b11 #b01))))

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)    
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax 2))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax 2))))

;; Expr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax #b01) ; #f
      (jne ,l0)
      (mov rax #b11) ; #t
      ,l0)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax #b01) ; compare to #f
      (je ,l0)       ; jump to c2 if #f
      ,@c1
      (jmp ,l1)      ; jump past c2
      ,l0
      ,@c2
      ,l1)))

;; Variable CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; Variable Expr Expr CEnv -> Asm
(define (compile-let x e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons x c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1)))

;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

;; Expr Expr CEnv -> Asm
(define (compile-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (sub rax (offset rsp ,(- (add1 (length c))))))))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define assert-integer
  `((mov rbx rax)
    (and rbx 1)
    (cmp rbx 0)
    (jne err)))
