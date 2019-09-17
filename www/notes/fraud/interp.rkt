#lang racket
(provide (all-defined-out))

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (let ((a (interp-env e r)))
       (interp-prim p a))]
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

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero?))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)))

;; Prim Answer -> Answer
(define (interp-prim p a)
  (match (list p a)
    [(list p 'err) 'err]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))
