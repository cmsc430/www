#lang racket
(provide (all-defined-out))

;; type Value =
;; ....
;; | '()
;; | (cons Value Value)


;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Integer
(define (interp-env e r)
  (match e
    [(list 'quote '()) '()]
    [(? value? v) v]
    [(list (? prim? p) es ...)
     (let ((as (interp-env* es r)))
       (interp-prim p as))]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]    
    [(? symbol? x)
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (interp-env e1 (ext r x v))])]))

;; [Listof Expr] REnv -> [Listof Answer]
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
     (cons (interp-env e r)
           (interp-env* es r))]))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 + - zero? cons car cdr))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)
      (null? x)
      (and (pair? x)
           (value? (car x))
           (value? (cdr x)))))

;; Prim [Listof Answer] -> Answer
(define (interp-prim p as)
  (match (cons p as)
    [(list p (? value?) ... 'err _ ...) 'err]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list '- (? integer? i0) (? integer? i1)) (- i0 i1)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (cons v0 v1)) v0]
    [(list 'cdr (cons v0 v1)) v1]
    [_ 'err]))

;; Env Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Variable Value -> Value
(define (ext r x i)
  (cons (list x i) r))
