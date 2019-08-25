#lang racket
(provide (all-defined-out))

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Integer
(define (interp-env e r)
  (match e
    [(? integer? i) i]
    [(? boolean? b) b]
    [`(add1 ,e0)
     (match (interp-env e0 r)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    [`(sub1 ,e0)
     (match (interp-env e0 r)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    [`(zero? ,e0)
     (match (interp-env e0 r)
       [(? integer? i) (zero? i)]
       [_ 'err])]
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
        (let ((r0 (ext r x v)))
          (interp-env e1 r0))])]
    [`(+ ,e0 ,e1)
     (match (interp-env e0 r)
       [(? integer? i0)
        (match (interp-env e1 r)
          [(? integer? i1)
           (+ i0 i1)]
          [_ 'err])]
       [_ 'err])]
    [`(- ,e0 ,e1)
     (match (interp-env e0 r)
       [(? integer? i0)
        (match (interp-env e1 r)
          [(? integer? i1)
           (- i0 i1)]
          [_ 'err])]
       [_ 'err])]))

;; Env Variable -> Integer
(define (lookup env x)
  (match env
    ['() (error "undefined variable:" x)]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Variable Integer -> Integer
(define (ext r x i)
  (cons (list x i) r))
