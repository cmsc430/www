#lang racket
(provide (all-defined-out))

(require "ast.rkt")

(define imm-shift        1)
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     #b0)
(define imm-val-true     #b11)
(define imm-val-false    #b01)

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
    [(int-e i)               (compile-integer i)]
    [(bool-e b)              (compile-boolean b)]
    [(var-e v)               (compile-variable v c)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-if p t f c)]
    [(let-e (list b) body)   (compile-let b body c)]))

;; Integer -> Asm
(define (compile-integer i)
  `((mov rax ,(arithmetic-shift i imm-shift))))

;; Boolean -> Asm
(define (compile-boolean b)
  `((mov rax ,(if b imm-val-true imm-val-false))))

(define (compile-prim p es c)
  (match (cons p es)
    [`(add1 ,e0)  (compile-add1 e0 c)]
    [`(sub1 ,e0)  (compile-sub1 e0 c)]
    [`(zero? ,e0) (compile-zero? e0 c)]
    [`(+ ,e0 ,e1) (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1) (compile-- e0 e1 c)]
    [_            (error
                    (format "prim applied to wrong number of args: ~a ~a" p es))]))

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

(define (compile-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (sub rax (offset rsp ,(- (add1 (length c))))))))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
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
(define (compile-let b e1 c)
  (match b
    [(binding x e0)
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
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define assert-integer
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx ,imm-type-int)
    (jne err)))
