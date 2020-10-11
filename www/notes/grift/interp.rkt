#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Integer
(define (interp-env e r)
  (match e
    [(var-e v) (lookup r v)]
    [(int-e i) i]
    [(bool-e b) b]
    [(prim-e (? prim? p) es)
         (let ((as (interp-env* es r)))
           (interp-prim p as))]
    [(if-e p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]    
    [(? symbol? x)
     (lookup r x)]
    [(let-e (list (binding x def)) body)
     (match (interp-env def r)
       ['err 'err]
       [v
        (interp-env body (ext r x v))])]))

(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es) (cons (interp-env e r) (interp-env* es r))]))

(define (interp-prim p as)
  (match (cons p as)
    [(list 'add1  (? integer? i)) (+ i 1)]
    [(list 'sub1  (? integer? i)) (- i 1)]
    [(list 'zero? (? integer? i)) (zero? i)]
    [(list '+     (? integer? i) (? integer? j)) (+ i j)]
    [(list '-     (? integer? i) (? integer? j)) (- i j)]
    [_            'err]))

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
